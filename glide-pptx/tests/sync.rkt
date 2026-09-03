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

  ;; And the result is still a program.
  (define out (open-output-string))
  (define code (parameterize ([current-output-port out] [current-error-port out]
                              [current-directory dir])
                 (system*/exit-code (find-system-path 'exec-file) (path->string program))))
  (check-equal? code 0 (format "the patched program still runs\n~a" (get-output-string out)))

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
  ;; And it is still a Rhombus program.
  (define out (open-output-string))
  (define code (parameterize ([current-output-port out] [current-error-port out]
                              [current-directory dir])
                 (system*/exit-code (find-system-path 'exec-file) (path->string program))))
  (check-equal? code 0 (format "the patched Rhombus program still runs\n~a"
                               (get-output-string out))))

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
