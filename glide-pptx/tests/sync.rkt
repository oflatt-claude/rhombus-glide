#lang racket/base
;; Two-way sync: a deck's edits merged back into the program that made it.
;;
;; The properties that matter are as much about restraint as about function. A
;; merge must change exactly the numbers that moved and nothing else in the
;; file, and it must refuse rather than guess when the thing it would patch is
;; not a literal.
(require rackunit racket/list racket/string racket/file racket/path racket/system
         racket/port racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/emit-rhombus
         glide-pptx/export
         glide-pptx/sync glide-pptx/sync-state glide-pptx/runtime "deck-edit.rkt"
         (only-in glide-pptx/watch program-picts))

;; Loads a program's slides the way the watcher does, in a fresh namespace, so
;; a re-export after a patch sees the patched source.
(define (program-picts-fresh path) (program-picts path))

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-sync"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; Sets up a program and a deck exported from it, with a base recorded.
(define (fixture name deck-name)
  (define dir (build-path work name))
  (make-directory* dir)
  (define pptx (build-path decks-dir deck-name))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define program (build-path dir "deck.rhm"))
  (write-rhombus-deck d program #:source-name (path->string pptx))
  (define exported (build-path dir "deck.pptx"))
  (define picts (load-program-picts program))
  (picts->pptx picts exported #:width (deck-width d) #:height (deck-height d))
  (define r (sync-once program exported #:workdir (build-path dir "syncwork")))
  (check-true (sync-report-base-written? r) "the first pass records a base")
  (check-equal? (sync-report-actions r) '() "and has nothing to merge")
  (values dir program exported))

;; ------------------------------------------- a drag comes back as a literal

(let ()
  (define-values (dir program exported) (fixture "drag" "05-realistic.pptx"))
  (define before (file->string program))
  (check-true (drag-in-deck! exported 3 "Rounded Rectangle 2" 100.0 200.0) "the shape to drag was found")
  (define r (sync-once program exported #:workdir (build-path dir "syncwork")))
  (define moves (filter (lambda (a) (eq? 'moved (sync-action-kind a)))
                        (sync-report-actions r)))
  (check-equal? (length moves) 1 "exactly one element moved")
  (check-equal? (sync-action-tag (first moves)) "Rounded Rectangle 2")
  (check-equal? (length (sync-report-applied r)) 1 "and it was applied")

  (define after (file->string program))
  ;; The restraint property: one line differs, and it is the right one.
  (define changed
    (for/list ([a (in-list (string-split before "\n"))]
               [b (in-list (string-split after "\n"))]
               #:unless (string=? a b))
      (cons a b)))
  (check-equal? (length changed) 1 "exactly one line of source changed")
  (check-true (regexp-match? #rx"57[.]6, 158[.]4" (car (first changed))) "was the old position")
  (check-true (regexp-match? #rx"100[.]0, 200[.]0" (cdr (first changed))) "is the new one")
  (check-equal? (length (string-split before "\n")) (length (string-split after "\n"))
                "no lines were added or removed")

  ;; And the result is still a program. Loading it is the check: running it
  ;; opens a slideshow, which needs a display.
  (check-true (pair? (load-program-picts program)) "the patched program still loads")

  ;; A second pass has nothing left to do, because the base moved with it.
  (define again (sync-once program exported #:workdir (build-path dir "syncwork")))
  (check-equal? (filter (lambda (a) (memq (sync-action-kind a) '(moved resized)))
                        (sync-report-actions again))
                '()
                "the sync converges: a second pass finds nothing"))

;; ------------------------------- a computed position becomes a correction

;; `left` is a variable, so there is no literal to replace. The drag is recorded
;; as a `#:nudge` on `at` instead, which keeps the program's own layout logic
;; and -- because it is one argument rather than a wrapper -- cannot stack up
;; when the element is dragged again.
(let ()
  (define dir (build-path work "computed"))
  (make-directory* dir)
  (define program (build-path dir "deck.rhm"))
  (call-with-output-file program #:exists 'replace
    (lambda (o)
      (write-string (string-join
                     '("#lang rhombus/and_meta"
                       "import:"
                       "  lib(\"glide-pptx/runtime.rhm\") open"
                       "export:"
                       "  all_slides"
                       "def left = 40.0"
                       "def slide_1 = slide_canvas("
                       "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
                       "  at(left, 60.0, ~tag: \"Box\","
                       "     shape_pict(~width: 100.0, ~height: 40.0,"
                       "                ~fill: hex(\"4472C4\")))"
                       ")"
                       "def all_slides = [slide_1]"
                       "")
                     "\n") o)))
  (define exported (build-path dir "deck.pptx"))
  (define (re-export!)
    (define picts (parameterize ([current-media-base dir])
                    (program-picts-fresh program)))
    (picts->pptx picts exported #:width 480.0 #:height 270.0))
  (re-export!)
  (sync-once program exported #:workdir (build-path dir "w"))

  ;; First drag: a correction appears.
  (check-true (drag-in-deck! exported 1 "Box" 200.0 100.0) "the shape to drag was found")
  (define before (file->string program))
  (define r1 (sync-once program exported #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r1)) 1 "the drag was applied")
  (define after1 (file->string program))
  (check-true (regexp-match? #rx"~nudge: [[]160[.]0, 40[.]0[]]" after1)
              (format "a correction of the right size was recorded:\n~a" after1))
  (check-equal? (length (regexp-match* #rx"~nudge" after1)) 1 "exactly one correction")
  (check-true (regexp-match? #rx"at[(]left, 60[.]0" after1)
              "and the program's own computed position is untouched")
  (check-equal? (length (string-split before "\n")) (length (string-split after1 "\n"))
                "no lines were added or removed")

  ;; The element now really draws where it was dragged to.
  (define states (program-slide-states program))
  (define box (findf (lambda (e) (equal? "Box" (el-state-tag e)))
                     (slide-state-elements (first states))))
  (check-true (and box #t) "the element is still found")
  (when box
    (check-= (el-state-x box) 200.0 0.01 "at the dragged x")
    (check-= (el-state-y box) 100.0 0.01 "and the dragged y"))

  ;; Second drag: the correction is updated in place, not nested.
  (re-export!)
  (sync-once program exported #:workdir (build-path dir "w"))
  (check-true (drag-in-deck! exported 1 "Box" 260.0 70.0) "the shape to drag was found")
  (define r2 (sync-once program exported #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r2)) 1 "the second drag was applied too")
  (define after2 (file->string program))
  (check-equal? (length (regexp-match* #rx"~nudge" after2)) 1
                (format "still exactly one correction, not a stack of them:\n~a" after2))
  (check-true (regexp-match? #rx"~nudge: [[]220[.]0, 10[.]0[]]" after2)
              (format "and it accumulated to the new offset:\n~a" after2))
  (define states2 (program-slide-states program))
  (define box2 (findf (lambda (e) (equal? "Box" (el-state-tag e)))
                      (slide-state-elements (first states2))))
  (when box2
    (check-= (el-state-x box2) 260.0 0.01 "drawing at the second dragged x")
    (check-= (el-state-y box2) 70.0 0.01 "and y")))

;; ------------------------------------------- the same thing, in Rhombus

;; A Rhombus program is patched the same way, which matters because that is the
;; language glide is written for. Shrubbery syntax objects carry source
;; locations, so the literals can be located and overwritten without
;; regenerating any of the surrounding text -- commas and line breaks included.
(let ()
  (define dir (build-path work "rhombus"))
  (make-directory* dir)
  (define pptx (build-path decks-dir "05-realistic.pptx"))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define program (build-path dir "deck.rhm"))
  (write-rhombus-deck d program #:source-name (path->string pptx))
  (define exported (build-path dir "deck.pptx"))
  (define picts (load-program-picts program))
  (picts->pptx picts exported #:width (deck-width d) #:height (deck-height d))
  (sync-once program exported #:workdir (build-path dir "w"))
  (define before (file->string program))
  (check-true (drag-in-deck! exported 3 "Rounded Rectangle 2" 111.0 222.0) "the shape to drag was found")
  (define r (sync-once program exported #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r)) 1 "the drag was applied to the .rhm")
  (define after (file->string program))
  (define changed
    (for/list ([a (in-list (string-split before "\n"))]
               [b (in-list (string-split after "\n"))]
               #:unless (string=? a b))
      (cons a b)))
  (check-equal? (length changed) 1 "exactly one line of Rhombus source changed")
  (check-true (regexp-match? #rx"at[(]111[.]0, 222[.]0," (cdr (first changed)))
              "with its commas intact")
  (check-true (regexp-match? #rx"~tag: \"Rounded Rectangle 2\"" (cdr (first changed)))
              "and the rest of the line untouched")
  ;; And it is still a Rhombus program. Loading it is the check: running it
  ;; opens a slideshow, which needs a display.
  (check-true (pair? (load-program-picts program)) "the patched program still loads"))

(printf "sync tests done; artifacts under ~a\n" work)

;; ------------------------------------- a tag is per slide, not per program

;; A real deck names shapes per slide, so "Title 1" exists on every slide. The
;; merge has to patch the slide that moved: keying sites by tag alone silently
;; rewrote slide 1 when slide 3 was dragged.
(let ()
  (define-values (dir program exported) (fixture "perslide" "01-placeholders.pptx"))
  (define before (file->string program))
  (define tags (for/list ([s (in-list (find-at-sites program))]) (at-site-tag s)))
  (check-equal? (length (filter (lambda (t) (string=? t "Title 1")) tags)) 3
                "the fixture does reuse one tag across slides")

  (check-true (drag-in-deck! exported 3 "Title 1" 111.0 222.0) "the shape to drag was found")
  (define r (sync-once program exported #:workdir (build-path dir "syncwork")))
  (define moves (filter (lambda (a) (eq? 'moved (sync-action-kind a)))
                        (sync-report-actions r)))
  (check-equal? (length moves) 1 "one element moved")
  (check-equal? (sync-action-slide (first moves)) 3 "on slide 3")
  (check-equal? (length (sync-report-applied r)) 1 "and it was applied")

  ;; The patched `at` is the one under `slide-3`, which is the last of the three.
  (define (title-lines text)
    (for/list ([l (in-list (string-split text "\n"))]
               #:when (regexp-match? #rx"~tag: \"Title 1\"" l))
      l))
  (define b (title-lines before))
  (define a (title-lines (file->string program)))
  (check-equal? (length a) 3)
  (check-equal? (first a) (first b) "slide 1's title is untouched")
  (check-equal? (second a) (second b) "slide 2's title is untouched")
  (check-not-equal? (third a) (third b) "slide 3's title moved")
  (check-true (and (regexp-match? #rx"111" (third a)) (regexp-match? #rx"222" (third a)))
              "to where it was dragged"))

;; ------------------------------------------ one tag, several elements

;; A tag names a *code site*, and one `at` in a loop draws several elements. So a
;; shared tag is not an error -- dragging all of them should move all of them,
;; because that is one correction on the one `at`. What cannot be expressed is an
;; edit to part of a family, and that is refused rather than guessed at.
(let ()
  (define dir (build-path work "family"))
  (make-directory* dir)
  (define program (build-path dir "loop.rhm"))
  (define deck (build-path dir "loop.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  for List (i: 0..3):"
            "    at(20.0 + i * 120.0, 60.0, ~tag: \"Box\","
            "       shape_pict(~width: 100.0, ~height: 60.0, ~shape: \"rect\","
            "                  ~fill: hex(\"4472C4\")))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))

  (define (xs) (for/list ([s (in-list (program-slide-states program))])
                 (map el-state-x (slide-state-elements s))))
  (define (at-line) (findf (lambda (l) (regexp-match? #rx"at[(]" l))
                           (string-split (file->string program) "\n")))

  ;; One `at`, three elements, one tag -- and the tag survives the round trip as
  ;; one tag rather than three invented ones.
  (reset!)
  (check-equal? (xs) '((20.0 140.0 260.0)) "the loop draws three")
  (define deck-tags
    (for/list ([s (in-list (deck-slide-states deck #:workdir (build-path dir "u")))])
      (map el-state-tag (slide-state-elements s))))
  (check-equal? deck-tags '(("Box" "Box" "Box"))
                "and they come back under the one tag they were drawn with")

  ;; Drag all of them the same way: one correction, all three move, spacing kept.
  (check-equal? (nudge-family-in-deck! deck 1 "Box" 50.0 30.0) 3 "all three were moved")
  (define r (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(moved))
  (check-equal? (length (sync-report-applied r)) 1 "one edit, not three")
  (check-regexp-match #rx"~nudge: [[]50[.]0, 30[.]0[]]" (at-line))
  (check-equal? (xs) '((70.0 190.0 310.0)) "every element moved, and only by that")

  ;; Drag one of them: there is no single correction that does this.
  (reset!)
  (check-true (drag-in-deck! deck 1 "Box" 400.0 400.0) "one was dragged")
  (define r2 (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (map sync-action-kind (sync-report-actions r2)) '(ambiguous))
  (check-equal? (sync-report-applied r2) '() "nothing was applied")
  (check-regexp-match #rx"did not all move the same way"
                      (cdr (first (sync-report-skipped r2))))
  (check-equal? (xs) '((20.0 140.0 260.0)) "and the program is untouched")

  ;; Delete one of them: a loop cannot say "all but that one".
  (reset!)
  (check-true (delete-from-deck! deck 1 "Box") "one was deleted")
  (define r3 (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (map sync-action-kind (sync-report-actions r3)) '(ambiguous))
  (check-equal? (sync-report-applied r3) '() "nothing was applied")
  (check-regexp-match #rx"deleting one of them" (cdr (first (sync-report-skipped r3))))
  (check-equal? (xs) '((20.0 140.0 260.0)) "and the program is untouched"))

;; Two `at` forms under one tag is a different thing: there is no way to tell
;; which of them an edit belongs to, so it is refused before anything is read.
(let ()
  (define dir (build-path work "twosites"))
  (make-directory* dir)
  (define program (build-path dir "two.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export:"
          "  all_slides"
          "def slide_1 = slide_canvas("
          "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
          "  at(20.0, 60.0, ~tag: \"Box\","
          "     shape_pict(~width: 100.0, ~height: 60.0, ~fill: hex(\"4472C4\"))),"
          "  at(200.0, 60.0, ~tag: \"Box\","
          "     shape_pict(~width: 100.0, ~height: 60.0, ~fill: hex(\"ED7D31\")))"
          ")"
          "def all_slides = [slide_1]"
          "")
    "\n")
   program #:exists 'replace)
  (define deck (build-path dir "two.pptx"))
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (void (sync-once program deck #:workdir (build-path dir "w")))
  (check-true (drag-in-deck! deck 1 "Box" 300.0 300.0))
  (check-exn #rx"appears 2 times"
             (lambda () (sync-once program deck #:workdir (build-path dir "w")))
             "two sites under one tag is refused"))

;; ------------------------------------------ shapes added and deleted in the editor

;; Drawing a new shape in PowerPoint or deleting one there has to reach the
;; program, or the next export would put it back -- the round trip would not be
;; one. An addition is written as source the same way the translator writes it;
;; a deletion takes the `at` form and its comment and nothing else.
(let ()
  (define-values (dir program exported) (fixture "addremove" "05-realistic.pptx"))
  (define before (file->string program))

  (check-equal? (add-shape-to-deck! exported 2 "New Rect 42" #:x 200.0 #:y 300.0
                                    #:width 120.0 #:height 80.0)
                "New Rect 42")
  (check-true (delete-from-deck! exported 3 "TextBox 10") "the shape to delete was found")

  (define r (sync-once program exported #:workdir (build-path dir "syncwork")))
  (define kinds (map sync-action-kind (sync-report-actions r)))
  (check-equal? (sort (map symbol->string kinds) string<?) '("added" "removed")
                "one addition and one deletion are seen")
  (check-equal? (length (sync-report-applied r)) 2 "and both are applied")

  ;; The addition reads like the rest of the file: a comment, then an `at`.
  (define after (file->string program))
  (check-regexp-match #rx"// New Rect 42 [(]id 9001[)]\n  at[(]200[.]0, 300[.]0, ~tag: \"New Rect 42\","
                      after)
  ;; The deletion leaves nothing behind -- no orphaned comment, no stray comma.
  (check-false (regexp-match? #rx"TextBox 10" after) "the deleted element is gone")
  (check-false (regexp-match? #rx",[ \t]*\n[ \t]*[)]" after) "and no dangling comma is left")

  ;; What matters is the invariant: the program now draws what the deck holds.
  (define prog-tags
    (for/list ([s (in-list (program-slide-states program))])
      (map el-state-tag (slide-state-elements s))))
  (define deck-tags
    (for/list ([s (in-list (deck-slide-states exported
                                              #:workdir (build-path dir "cmp")))])
      (map el-state-tag (slide-state-elements s))))
  (check-equal? prog-tags deck-tags "program and deck hold the same elements, in order")

  ;; And a second pass has nothing to do, so the edit converged.
  (define again (sync-once program exported #:workdir (build-path dir "syncwork")))
  (check-equal? (sync-report-actions again) '() "the merge converged")

  ;; The file still parses and still exports.
  (check-equal? (length (load-program-picts program)) 3 "the patched program still runs")
  (check-true (> (length (string-split after "\n")) 0))
  (check-not-equal? before after))

;; --------------------------------------- the merge does not pair slides by index

;; It used to, and taking the difference between two unrelated slides for edits
;; was actively destructive: one slide pasted at the front of a three-slide deck
;; produced 28 "edits" and deleted eight real elements from the program. Slides
;; are matched on their contents now, so an inserted slide does not shift the
;; ones after it.
(let ()
  (define-values (dir program exported) (fixture "slideset" "05-realistic.pptx"))
  (define other (build-path decks-dir "03-shapes.pptx"))
  (check-equal? (paste-slide! exported other 1) 4 "a slide was pasted in")
  (check-true (move-slide! exported 4 1) "at the front, so every later index shifts")

  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  ;; The one thing that changed, and nothing else.
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(added-slide)
                "only the new slide is reported")
  (define removed (filter (lambda (a) (eq? 'removed (sync-action-kind a)))
                          (sync-report-actions r)))
  (check-equal? removed '() "no element is thought to have been deleted"))

;; A conflict and a text edit both used to raise an arity error rather than being
;; reported: `sync-action` takes five fields and those two calls passed four.
;; Nothing exercised them, so editing text in PowerPoint always crashed.
(let ()
  (define-values (dir program exported) (fixture "retext" "05-realistic.pptx"))
  (check-true (retext-in-deck! exported 3 "TextBox 1" "Rewritten") "the text was changed")
  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (define texts (filter (lambda (a) (eq? 'retext (sync-action-kind a)))
                        (sync-report-actions r)))
  (check-equal? (length texts) 1 "a text edit is reported, not raised")
  (check-equal? (sync-action-tag (first texts)) "TextBox 1")
  (check-equal? (length (sync-report-applied r)) 1 "and applied")
  (check-regexp-match #rx"\"Rewritten\"" (file->string program)
                      "the string literal in the source was replaced")
  (define again (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (sync-report-actions again) '() "and it converged"))

;; ------------------------------------------------ slides added in the editor

;; Pasting slides in from another deck. The program is where slides live, so a
;; pasted slide has to become a `def slide_N` and an entry in `all_slides` --
;; and the deck it came from has to survive a re-export unchanged, which is the
;; whole point of the round trip.
(define (deck-shape pptx dir tag)
  (for/list ([s (in-list (deck-slide-states pptx #:workdir (build-path dir tag)))])
    (map el-state-tag (slide-state-elements s))))

(let ()
  (define-values (dir program exported) (fixture "addslide" "05-realistic.pptx"))
  (define other (build-path decks-dir "03-shapes.pptx"))

  (check-equal? (paste-slide! exported other 1) 4 "a slide was pasted in at the end")
  (define shape-after-paste (deck-shape exported dir "s1"))
  (check-equal? (length shape-after-paste) 4 "the deck has four slides")

  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(added-slide)
                "the new slide is the only thing to merge")
  (check-equal? (length (sync-report-applied r)) 1 "and it was applied")

  ;; It reads like the rest of the file: a rule, a name, a canvas.
  (define src (file->string program))
  (check-regexp-match #rx"def slide_4 = slide_canvas[(]" src)
  (check-regexp-match #rx"all_slides = [[]slide_1, slide_2, slide_3, slide_4[]]" src)
  (check-regexp-match #rx"\n  slide_4\n" src "and it is exported like the others")

  ;; The program now draws four slides, and re-exporting reproduces the deck
  ;; the editor holds -- same elements, same order.
  (check-equal? (length (load-program-picts program)) 4 "the program builds four slides")
  (define again (build-path dir "again.pptx"))
  (picts->pptx (load-program-picts program) again #:width 959.976 #:height 540.0)
  (check-equal? (deck-shape again dir "s2") shape-after-paste
                "the re-exported deck holds what the editor held")

  ;; And a second pass has nothing to do.
  (check-equal? (sync-report-actions
                 (sync-once program exported #:workdir (build-path dir "w2")))
                '()
                "the merge converged"))

;; Where it lands matters: a slide pasted at the front belongs at the front of
;; `all_slides`. The definition itself goes after the last one -- `all_slides`
;; carries the order, so nothing has to be renumbered or reflowed.
(let ()
  (define-values (dir program exported) (fixture "addfront" "05-realistic.pptx"))
  (check-equal? (paste-slide! exported (build-path decks-dir "03-shapes.pptx") 1) 4)
  (check-true (move-slide! exported 4 1) "and moved to the front")
  (define shape (deck-shape exported dir "s1"))

  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (length (sync-report-applied r)) 1 "the paste was applied")
  (check-regexp-match #rx"all_slides = [[]slide_4, slide_1, slide_2, slide_3[]]"
                      (file->string program)
                      "the new slide is first in the order")

  (define again (build-path dir "again.pptx"))
  (picts->pptx (load-program-picts program) again #:width 959.976 #:height 540.0)
  (check-equal? (deck-shape again dir "s2") shape
                "and the re-export has the slides in that order"))

;; Two at once, and one of them carrying an image, which has to be copied next
;; to the program or the program cannot draw it.
(let ()
  (define-values (dir program exported) (fixture "addtwo" "05-realistic.pptx"))
  (check-equal? (paste-slide! exported (build-path decks-dir "03-shapes.pptx") 1) 4)
  (check-equal? (paste-slide! exported (build-path decks-dir "04-pictures-groups.pptx") 1) 5)
  (define shape (deck-shape exported dir "s1"))
  (check-equal? (length shape) 5 "five slides now")

  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (length (sync-report-applied r)) 2 "both were applied")
  (define src (file->string program))
  (check-regexp-match #rx"all_slides = [[]slide_1, slide_2, slide_3, slide_4, slide_5[]]" src)

  (check-equal? (length (load-program-picts program)) 5 "the program builds five slides")
  (define again (build-path dir "again.pptx"))
  (picts->pptx (load-program-picts program) again #:width 959.976 #:height 540.0)
  (check-equal? (deck-shape again dir "s2") shape "and the deck round-trips"))

;; Deleting a slide is not merged back yet, and it says so rather than taking
;; the difference for edits.
(let ()
  (define-values (dir program exported) (fixture "delslide" "05-realistic.pptx"))
  (define before (file->string program))
  (check-true (delete-slide! exported 2) "a slide was deleted in the editor")
  (define msg (with-handlers ([exn:fail? exn-message])
                (sync-once program exported #:workdir (build-path dir "w2"))))
  (check-regexp-match #rx"not in the deck any more" msg)
  (check-regexp-match #rx"all_slides" msg "and says what to do about it")
  (check-equal? (file->string program) before "the program is untouched"))

;; ------------------------------------------------- a program holding a group

;; Reported from a live session as a crash: `it:shape-path-box` given an
;; `it:group`. The state builder's last branch assumed anything left was a
;; shape-path, and a group became a semantic item when groups started exporting
;; as groups -- so it was read as one. Nothing here had ever put a program with a
;; group through the sync.
(let ()
  (define-values (dir program exported) (fixture "grouped" "04-pictures-groups.pptx"))
  (define states (program-slide-states program))
  (check-true (pair? states) "a program with a group has slide states")
  (define kinds
    (remove-duplicates
     (append* (for/list ([s (in-list states)])
                (map el-state-kind (slide-state-elements s))))))
  (check-true (and (memq 'group kinds) #t)
              (format "and a group among them: ~s" kinds))

  ;; And it syncs: the group is one element to drag.
  (define r (sync-once program exported #:workdir (build-path dir "w2")))
  (check-equal? (sync-report-actions r) '() "nothing to merge at rest")
  (define found
    (for*/first ([s (in-list states)]
                 [e (in-list (slide-state-elements s))]
                 #:when (eq? 'group (el-state-kind e)))
      (cons (slide-state-index s) (el-state-tag e))))
  (check-true (and found #t) "the group has a tag")
  (when found
    (check-true (drag-in-deck! exported (car found) (cdr found) 120.0 140.0)
                "the group was dragged")
    (define r2 (sync-once program exported #:workdir (build-path dir "w2")))
    (check-equal? (map sync-action-kind (sync-report-actions r2)) '(moved)
                  "and the drag comes back as a move, on the group itself")
    (check-equal? (sync-action-tag (first (sync-report-actions r2))) (cdr found))))

;; ============================================ the editing workflow, end to end

;; The single edits each have their own test above. What those do not cover is
;; the workflow: several edits in one pass, the same element edited twice, an
;; element added and then moved, and rounds of this in a row. Each round asserts
;; the same two things -- the program draws what the editor holds, and a second
;; pass has nothing left to do.

;; What the program and the deck each say is on a slide, as tag -> geometry, so
;; the two can be compared without a renderer.
;; The five numbers rounded, and the two flips as they are -- `el-geometry` ends
;; in booleans, which do not round.
(define (shape-of e)
  (cons (el-state-tag e)
        (append (for/list ([v (in-list (take (el-geometry e) 5))])
                  (/ (round (* 10.0 v)) 10.0))
                (list (and (el-state-flip-h? e) #t) (and (el-state-flip-v? e) #t)))))

(define (program-shape program)
  (for/list ([s (in-list (program-slide-states program))])
    (map shape-of (slide-state-elements s))))

(define (deck-shape-of pptx dir tag)
  (for/list ([s (in-list (deck-slide-states pptx #:workdir (build-path dir tag)))])
    (map shape-of (slide-state-elements s))))

;; One round: apply `edit!` to the deck, merge, and check that the program now
;; agrees with the deck and that the merge has settled.
(define (round! name dir program exported edit! [expect #f])
  (check-true (edit!) (format "~a: the edit went into the deck" name))
  (define r (sync-once program exported #:workdir (build-path dir "w")))
  (when expect
    (check-equal? (sort (map symbol->string (map sync-action-kind (sync-report-actions r)))
                        string<?)
                  (sort (map symbol->string expect) string<?)
                  (format "~a: what the merge saw" name)))
  (check-equal? (sync-report-skipped r) '()
                (format "~a: every action was applied" name))
  (check-equal? (program-shape program) (deck-shape-of exported dir (format "~a-cmp" name))
                (format "~a: the program now draws what the deck holds" name))
  (define again (sync-once program exported #:workdir (build-path dir "w")))
  (check-equal? (sync-report-actions again) '()
                (format "~a: and the merge settled" name))
  r)

(let ()
  (define-values (dir program exported) (fixture "workflow" "05-realistic.pptx"))

  ;; Move.
  (round! "move" dir program exported
          (lambda () (drag-in-deck! exported 3 "Rounded Rectangle 2" 90.0 210.0))
          '(moved))

  ;; Resize -- the size is a literal on the leaf, not on `at`.
  (round! "resize" dir program exported
          (lambda () (resize-in-deck! exported 3 "Rounded Rectangle 2" 200.0 100.0))
          '(resized))

  ;; Retype the text.
  (round! "retext" dir program exported
          (lambda () (retext-in-deck! exported 3 "TextBox 1" "Rewritten"))
          '(retext))
  (check-regexp-match #rx"\"Rewritten\"" (file->string program))

  ;; Move and retype in one pass, on different elements.
  (round! "move+retext" dir program exported
          (lambda () (and (drag-in-deck! exported 3 "Right Arrow 3" 300.0 320.0)
                          (retext-in-deck! exported 3 "TextBox 1" "Twice")))
          '(moved retext))

  ;; Add a shape, then move the shape that was added.
  (round! "add" dir program exported
          (lambda () (and (add-shape-to-deck! exported 3 "New Box" #:x 40.0 #:y 400.0) #t))
          '(added))
  (round! "move the added one" dir program exported
          (lambda () (drag-in-deck! exported 3 "New Box" 120.0 430.0))
          '(moved))

  ;; Delete it again.
  (round! "delete" dir program exported
          (lambda () (delete-from-deck! exported 3 "New Box"))
          '(removed))
  (check-false (regexp-match? #rx"New Box" (file->string program))
               "the deleted element left no trace in the source")

  ;; Several elements moved at once.
  (round! "three at once" dir program exported
          (lambda () (and (drag-in-deck! exported 3 "Rounded Rectangle 2" 60.0 180.0)
                          (drag-in-deck! exported 3 "Rounded Rectangle 4" 260.0 180.0)
                          (drag-in-deck! exported 3 "Rounded Rectangle 6" 460.0 180.0)))
          '(moved moved moved))

  ;; And rounds of it, to see that nothing accumulates.
  (for ([i (in-range 3)])
    (round! (format "round ~a" i) dir program exported
            (lambda () (drag-in-deck! exported 3 "Right Arrow 5"
                                      (+ 200.0 (* i 30.0)) (+ 250.0 (* i 20.0))))
            '(moved)))

  ;; The file is still a program, and still the same one.
  (check-equal? (length (load-program-picts program)) 3 "three slides throughout"))

;; ------------------------------------------ rotating and mirroring an element

;; Dragging a line's endpoint past the other end mirrors the shape rather than
;; moving it, and rotating one turns it. Both were dropped: a rotation was
;; counted as applied while nothing was written -- there was no `~rotate:` to
;; write to and none was added -- and a mirror was not in the state a merge
;; compares, so it was never noticed at all.
(let ()
  (define dir (build-path work "turned"))
  (make-directory* dir)
  (define program (build-path dir "t.rhm"))
  (define deck (build-path dir "t.pptx"))
  (define (write-program! extra-at extra-leaf)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            (format "  at(100.0, 100.0, ~a~~tag: \"Line\"," extra-at)
            "     shape_pict(~width: 200.0, ~height: 120.0,"
            (format "                ~a~~shape: \"straightConnector1\"," extra-leaf)
            "                ~line: make_stroke(hex(\"000000\"), ~width: 3.0)))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))

  ;; Neither stated: both have to be added to the source.
  (write-program! "" "")
  (check-true (rotate-in-deck! deck 1 "Line" 30.0) "the line was rotated")
  (define r (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r)) 1 "the rotation was applied")
  (check-regexp-match #rx"~rotate: 30[.]0" (file->string program)
                      "and written into the source, which had none")
  (check-equal? (sync-report-actions
                 (sync-once program deck #:workdir (build-path dir "w")))
                '() "and it settled")

  (write-program! "" "")
  (check-true (edit-after-tag! deck 1 "Line" #px"<a:xfrm" "<a:xfrm flipH=\"1\"")
              "the line was mirrored")
  (define r2 (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r2)) 1 "the mirror was applied")
  (check-regexp-match #rx"~flip_h: #true" (file->string program)
                      "and written into the leaf, which had none")
  (check-equal? (sync-report-actions
                 (sync-once program deck #:workdir (build-path dir "w")))
                '() "and it settled")

  ;; Both stated: the values have to change, not be added again.
  (write-program! "~rotate: 20.0, " "~flip_h: #true, ")
  (check-true (rotate-in-deck! deck 1 "Line" 50.0))
  (check-true (edit-after-tag! deck 1 "Line" #px" flipH=\"1\"" "") "and un-mirrored")
  (define r3 (sync-once program deck #:workdir (build-path dir "w")))
  (check-equal? (length (sync-report-applied r3)) 1 "one action for both")
  (define src (file->string program))
  (check-regexp-match #rx"~rotate: 50[.]0" src "the rotation was changed in place")
  (check-regexp-match #rx"~flip_h: #false" src "and so was the mirror")
  (check-equal? (length (regexp-match* #rx"~rotate:" src)) 1 "no second rotation")
  (check-equal? (length (regexp-match* #rx"~flip_h:" src)) 1 "no second mirror"))

;; ------------------------------------------------ recolouring and restyling

;; Appearance is the code's, and it used to be simply dropped: recolouring a
;; shape in the editor vanished without a word. It is reported now, and written
;; where the source states it as a literal.
;;
;; A colour with a name is different. It belongs to everything that uses it, so
;; it is rewritten only when everything that uses it changed the same way --
;; which is the rule a repeated tag already follows.
(let ()
  (define dir (build-path work "restyle"))
  (make-directory* dir)
  (define program (build-path dir "r.rhm"))
  (define deck (build-path dir "r.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def brand = hex(\"4472C4\")"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  at(60.0, 60.0, ~tag: \"Plain\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"))),"
            "  at(240.0, 60.0, ~tag: \"One\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: brand)),"
            "  at(400.0, 60.0, ~tag: \"Two\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: brand)),"
            "  at(60.0, 200.0, ~tag: \"Words\","
            "     textbox(~width: 300.0, ~height: 60.0,"
            "             para(run(\"hello\", ~font: \"Arial\", ~size: 24.0))))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))
  (define (recolour! tag hex)
    (edit-after-tag! deck 1 tag #px"<a:srgbClr val=\"[0-9A-Fa-f]+\"/>"
                     (format "<a:srgbClr val=\"~a\"/>" hex)))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w")))

  ;; A literal colour is rewritten where it stands.
  (reset!)
  (check-true (recolour! "Plain" "70AD47") "the shape was recoloured")
  (define r (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r)) '(restyle)
                "and that is reported as a restyle, not passed over")
  (check-equal? (length (sync-report-applied r)) 1 "and applied")
  (check-regexp-match #rx"~fill: hex[(]\"70AD47\"[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A named colour, changed on one of the two that use it: refused, and the
  ;; report says which name and how many did not change with it.
  (reset!)
  (check-true (recolour! "One" "70AD47"))
  (define r2 (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r2)) '(restyle))
  (check-equal? (sync-report-applied r2) '() "nothing was written")
  (check-regexp-match #rx"brand" (cdr (first (sync-report-skipped r2)))
                      "and the report names the colour")
  (check-regexp-match #rx"1 other element that did not change"
                      (cdr (first (sync-report-skipped r2))))
  (check-regexp-match #rx"def brand = hex[(]\"4472C4\"[)]" (file->string program)
                      "the definition is untouched")

  ;; Both of them: the definition is rewritten, once.
  (reset!)
  (check-true (recolour! "One" "70AD47"))
  (check-true (recolour! "Two" "70AD47"))
  (define r3 (sync!))
  (check-equal? (length (sync-report-applied r3)) 2 "both are applied")
  (define src (file->string program))
  (check-regexp-match #rx"def brand = hex[(]\"70AD47\"[)]" src "through the definition")
  (check-equal? (length (regexp-match* #rx"70AD47" src)) 1 "which is written once")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A font and a size on a single-run body.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"typeface=\"[^\"]*\"" "typeface=\"Courier New\""))
  (check-true (edit-after-tag! deck 1 "Words" #px"sz=\"[0-9]+\"" "sz=\"4000\""))
  (define r4 (sync!))
  (check-equal? (length (sync-report-applied r4)) 1 "font and size in one action")
  (define src4 (file->string program))
  (check-regexp-match #rx"~font: \"Courier New\"" src4)
  (check-regexp-match #rx"~size: 40[.]0" src4)
  (check-equal? (sync-report-actions (sync!)) '() "and it settled"))

;; ------------------------------------------- appearance the source never states

;; Most of what an editor does to a shape's appearance is not a value the
;; program already has: a solid line has no `~dash:` to rewrite, and a shape
;; with no fill has no `~fill:` at all. Reporting those is worse than useless --
;; the deck and the program disagree and the user is told to fix it by hand --
;; so the argument is added, and one that the editor took away is removed.
(let ()
  (define dir (build-path work "appearance"))
  (make-directory* dir)
  (define program (build-path dir "p.rhm"))
  (define deck (build-path dir "p.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  at(60.0, 60.0, ~tag: \"Box\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"),"
            "                ~line: make_stroke(hex(\"203040\"), ~width: 2.0))),"
            "  at(240.0, 60.0, ~tag: \"Bare\","
            "     shape_pict(~width: 90.0, ~height: 60.0)),"
            "  at(60.0, 200.0, ~tag: \"Words\","
            "     textbox(~width: 300.0, ~height: 60.0,"
            "             para(run(\"hello\", ~font: \"Arial\", ~size: 24.0,"
            "                      ~color: hex(\"101010\"))))),"
            "  at(60.0, 300.0, ~tag: \"Plain\","
            "     textbox(~width: 300.0, ~height: 60.0, ~anchor: #'top,"
            "             para(~align: #'left, run(\"plain\", ~size: 18.0))))"
            ")"
            "def all_slides = [slide_1]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w")))
  (define (applied! r why) (check-equal? (length (sync-report-applied r)) 1 why))
  ;; Keynote writes a colour into the run's properties, whether or not the run
  ;; had one.
  (define (recolour-run! tag hex)
    (or (edit-after-tag! deck 1 tag #px"<a:srgbClr val=\"[0-9A-Fa-f]+\"/></a:solidFill>"
                         (format "<a:srgbClr val=\"~a\"/></a:solidFill>" hex))
        (edit-after-tag! deck 1 tag #px"(<a:rPr[^>]*)/>"
                         (format "\\1><a:solidFill><a:srgbClr val=\"~a\"/></a:solidFill></a:rPr>" hex))))

  ;; A dash on a line that is solid: the argument is added inside the stroke.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Box" #px"</a:ln>"
                               "<a:prstDash val=\"dash\"/></a:ln>")
              "the line was dashed")
  (applied! (sync!) "and the dash was written")
  (check-regexp-match #rx"~dash: #'dash" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A fill made translucent: the alpha is added inside its own `hex`.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Box" #px"<a:srgbClr val=\"ED7D31\"/>"
               "<a:srgbClr val=\"ED7D31\"><a:alpha val=\"50000\"/></a:srgbClr>")
              "the fill was made translucent")
  (applied! (sync!) "and the opacity was written")
  (check-regexp-match #rx"hex[(]\"ED7D31\", ~alpha: 0[.]5[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; An outline on a shape that has none: a whole stroke, with the width the
  ;; editor gave it, since neither has anywhere to be written on its own.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Bare" #px"<a:ln><a:noFill/></a:ln>"
               "<a:ln w=\"19050\"><a:solidFill><a:srgbClr val=\"FF0000\"/></a:solidFill></a:ln>")
              "the shape was given an outline")
  (applied! (sync!) "and the stroke was written")
  (check-regexp-match #rx"~line: make_stroke[(]hex[(]\"FF0000\"[)], ~width: 1[.]5[)]"
                      (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A fill on a shape that has none.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Bare" #px"<a:noFill/><a:ln>"
               "<a:solidFill><a:srgbClr val=\"00B050\"/></a:solidFill><a:ln>")
              "the shape was given a fill")
  (applied! (sync!) "and the fill was written")
  (check-regexp-match #rx"~fill: hex[(]\"00B050\"[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; And taken away again: `#false` is how the program says a shape has none,
  ;; and the whole argument goes, not the colour inside it.
  (check-true (edit-after-tag!
               deck 1 "Bare" #px"<a:solidFill><a:srgbClr val=\"00B050\"/></a:solidFill>"
               "<a:noFill/>")
              "the fill was removed")
  (applied! (sync!) "and the removal was written")
  (check-regexp-match #rx"~fill: #false" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; The outline of the shape that has one.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Box" #px"<a:ln w=\"[0-9]+\"[^>]*>.*?</a:ln>"
                               "<a:ln><a:noFill/></a:ln>")
              "the outline was removed")
  (applied! (sync!) "and the removal was written")
  (define src (file->string program))
  (check-regexp-match #rx"~line: #false" src)
  (check-false (regexp-match? #rx"make_stroke" src) "the stroke call is gone")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Bold and italic are flags: absent means false, so they are added.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"<a:rPr lang=\"en-US\" sz=\"2400\""
                               "<a:rPr lang=\"en-US\" sz=\"2400\" b=\"1\" i=\"1\"")
              "the text was bolded and italicised")
  (applied! (sync!) "and both were written")
  (define src2 (file->string program))
  (check-regexp-match #rx"~bold: #true" src2)
  (check-regexp-match #rx"~italic: #true" src2)
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A colour the source never states for its text.
  (reset!)
  (check-true (recolour-run! "Plain" "CC0000") "the text was recoloured")
  (applied! (sync!) "and the colour was written")
  (check-regexp-match #rx"~color: hex[(]\"CC0000\"[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Centring text is the first thing anyone does in an editor. The paragraph
  ;; does not state an alignment, so one is added.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"<a:pPr algn=\"l\"" "<a:pPr algn=\"ctr\"")
              "the text was centred")
  (applied! (sync!) "and the alignment was written")
  (check-regexp-match #rx"~align: #'center" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; And where it does state one, that one changes -- rather than a second
  ;; `~align:` appearing beside it, which would not compile.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Plain" #px"<a:pPr algn=\"l\"" "<a:pPr algn=\"r\"")
              "the text was right-aligned")
  (applied! (sync!) "and the alignment was written")
  (define srca (file->string program))
  (check-regexp-match #rx"~align: #'right" srca)
  (check-equal? (length (regexp-match* #rx"~align:" srca)) 1
                "the one the paragraph already had, and no second one beside it")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Line spacing is a pair, so the whole value is written.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"<a:spcPct val=\"100000\"/>"
                               "<a:spcPct val=\"150000\"/>")
              "the lines were spaced out")
  (applied! (sync!) "and the spacing was written")
  (check-regexp-match #rx"~line_spacing: pair[(]#'percent, 1[.]5[)]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Space before and after a paragraph, in points.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"</a:lnSpc>"
                               (string-append "</a:lnSpc><a:spcBef><a:spcPts val=\"1200\"/></a:spcBef>"
                                              "<a:spcAft><a:spcPts val=\"600\"/></a:spcAft>"))
              "the paragraph was given room")
  (applied! (sync!) "and both were written")
  (define srcs (file->string program))
  (check-regexp-match #rx"~space_before: 12[.]0" srcs)
  (check-regexp-match #rx"~space_after: 6[.]0" srcs)
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A dash the source already states: `#'dash` is a quote and a name, and the
  ;; two of them together are what gets rewritten.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Box" #px"</a:ln>"
                               "<a:prstDash val=\"dash\"/></a:ln>"))
  (applied! (sync!) "the dash was added")
  (check-true (edit-after-tag! deck 1 "Box" #px"<a:prstDash val=\"dash\"/>"
                               "<a:prstDash val=\"sysDot\"/>")
              "and then changed")
  (applied! (sync!) "and the change was written")
  (define srcd (file->string program))
  (check-equal? (length (regexp-match* #rx"~dash:" srcd)) 1 "in place, not beside")
  (check-false (regexp-match? #rx"~dash: #'dash" srcd) "and it is not the old one")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Where the text sits in its box, which the editor's inspector changes
  ;; without touching a word of the text. None of it is stated, so all of it is
  ;; added.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Words" #px"anchor=\"t\"" "anchor=\"ctr\"")
              "the text was anchored to the middle")
  (check-true (edit-after-tag! deck 1 "Words" #px"wrap=\"square\"" "wrap=\"none\"")
              "and wrapping turned off")
  (check-true (edit-after-tag! deck 1 "Words" #px"<a:noAutofit/>" "<a:normAutofit/>")
              "and set to shrink on overflow")
  (check-true (edit-after-tag! deck 1 "Words" #px"lIns=\"91440\"" "lIns=\"228600\"")
              "and given a wider inset")
  (applied! (sync!) "all four in one action")
  (define srcb (file->string program))
  (check-regexp-match #rx"~anchor: #'center" srcb)
  (check-regexp-match #rx"~wrap: #false" srcb)
  (check-regexp-match #rx"~autofit: #'shrink" srcb)
  (check-regexp-match #rx"~insets: insets[(]18[.]0, 3[.]6, 7[.]2, 3[.]6[)]" srcb)
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; The box that states its anchor has that one changed.
  (reset!)
  (check-true (edit-after-tag! deck 1 "Plain" #px"anchor=\"t\"" "anchor=\"b\"")
              "the text was anchored to the bottom")
  (applied! (sync!) "and written")
  (define srcp (file->string program))
  (check-regexp-match #rx"~anchor: #'bottom" srcp)
  (check-equal? (length (regexp-match* #rx"~anchor:" srcp)) 1 "in place")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A gradient the source does stand behind: it cannot be told which stops the
  ;; editor picked, but a plain colour replaces the whole of it.
  (reset!)
  (display-to-file
   (regexp-replace #rx"~fill: hex[(]\"ED7D31\"[)]" (file->string program)
                   "~fill: gradient_fill([pair(0.0, hex(\"FF0000\")), pair(1.0, hex(\"0000FF\"))], 0.0)")
   program #:exists 'replace)
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (void (sync!))
  (check-true (edit-after-tag! deck 1 "Box" #px"<a:gradFill.*?</a:gradFill>"
                               "<a:solidFill><a:srgbClr val=\"00B050\"/></a:solidFill>")
              "the gradient was made a plain colour")
  (applied! (sync!) "and the whole fill was written")
  (define srcg (file->string program))
  (check-regexp-match #rx"~fill: hex[(]\"00B050\"[)]" srcg)
  (check-false (regexp-match? #rx"gradient_fill" srcg) "the gradient call is gone")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; A gradient is not a colour the source can be told to be, and saying so is
  ;; the point: the alternative is writing one stop of it and calling it done.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Box" #px"<a:solidFill><a:srgbClr val=\"ED7D31\"/></a:solidFill>"
               (string-append "<a:gradFill><a:gsLst>"
                              "<a:gs pos=\"0\"><a:srgbClr val=\"FF0000\"/></a:gs>"
                              "<a:gs pos=\"100000\"><a:srgbClr val=\"0000FF\"/></a:gs>"
                              "</a:gsLst><a:lin ang=\"0\"/></a:gradFill>"))
              "the fill was made a gradient")
  (define rg (sync!))
  (check-equal? (sync-report-applied rg) '() "which is not written")
  (check-regexp-match #rx"~fill: hex[(]\"ED7D31\"[)]" (file->string program)
                      "and the fill it could not write is left as it was")
  (check-regexp-match #rx"gradient" (cdr (first (sync-report-skipped rg)))
                      "and the report says it was made a gradient"))

;; --------------------------------- the rest of what an editor can do to a deck

;; Surveyed by simulating each action and seeing what the merge made of it. Four
;; were silent -- a dashed line, a translucent fill, bringing a shape to the
;; front, reordering the slides -- and one was worse than silent: duplicating a
;; shape wrote a second `at` under the same tag, which left the program in a
;; state no later sync could read.
(let ()
  (define dir (build-path work "actions"))
  (make-directory* dir)
  (define program (build-path dir "a.rhm"))
  (define deck (build-path dir "a.pptx"))
  (define (reset!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "export:"
            "  all_slides"
            "def slide_1 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  at(60.0, 60.0, ~tag: \"Box\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"),"
            "                ~line: make_stroke(hex(\"203040\"), ~width: 2.0))),"
            "  at(240.0, 60.0, ~tag: \"Other\","
            "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"4472C4\")))"
            ")"
            "def slide_2 = slide_canvas("
            "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
            "  at(60.0, 60.0, ~tag: \"Second\","
            "     shape_pict(~width: 100.0, ~height: 60.0, ~fill: hex(\"70AD47\")))"
            ")"
            "def all_slides = [slide_1, slide_2]"
            "")
      "\n")
     program #:exists 'replace)
    (define b (base-path-for program))
    (when (file-exists? b) (delete-file b))
    (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
    (void (sync-once program deck #:workdir (build-path dir "w"))))
  (define (sync!) (sync-once program deck #:workdir (build-path dir "w")))

  ;; A line's colour and width are literals inside `make_stroke`.
  (reset!)
  (check-true (edit-after-tag!
               deck 1 "Box"
               #px"<a:ln[^>]*><a:solidFill><a:srgbClr val=\"[0-9A-Fa-f]+\"/>"
               "<a:ln w=\"76200\"><a:solidFill><a:srgbClr val=\"FF0000\"/>"))
  (define r (sync!))
  (check-equal? (length (sync-report-applied r)) 1 "the line was restyled")
  (define src (file->string program))
  (check-regexp-match #rx"make_stroke[(]hex[(]\"FF0000\"[)]" src "its colour written")
  (check-regexp-match #rx"~width: 6[.]0" src "and its width")
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Bringing a shape to the front is reported, since rewriting it would mean
  ;; moving the `at` form rather than a literal in it.
  (reset!)
  (check-true (bring-to-front! deck 1 "Box") "the shape was brought to the front")
  (define r2 (sync!))
  (check-true (and (memq 'restacked (map sync-action-kind (sync-report-actions r2))) #t)
              "the drawing order is reported")
  (check-regexp-match #rx"move its `at` form"
                      (cdr (first (sync-report-skipped r2)))
                      "and the report says what to do about it")

  ;; Reordering the slides is `all_slides`, which is a literal list.
  (reset!)
  (check-true (move-slide! deck 2 1) "the slides were reordered")
  (define r3 (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r3)) '(reordered))
  (check-equal? (length (sync-report-applied r3)) 1 "and applied")
  (check-regexp-match #rx"all_slides = [[]slide_2, slide_1[]]" (file->string program))
  (check-equal? (sync-report-actions (sync!)) '() "and it settled")

  ;; Duplicating a shape gives the copy a name of its own, so the program stays
  ;; readable.
  (reset!)
  (check-true (duplicate-in-deck! deck 1 "Other") "the shape was duplicated")
  (define r4 (sync!))
  (check-equal? (map sync-action-kind (sync-report-actions r4)) '(added))
  (check-equal? (length (sync-report-applied r4)) 1 "the copy was added")
  (check-regexp-match #rx"~tag: \"Other [(]2[)]\"" (file->string program)
                      "under a name of its own")
  (check-equal? (sync-report-actions (sync!)) '()
                "and the program is still one a sync can read"))
