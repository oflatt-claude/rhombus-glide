#lang racket/base
;; Two-way sync: a deck's edits merged back into the program that made it.
;;
;; The properties that matter are as much about restraint as about function. A
;; merge must change exactly the numbers that moved and nothing else in the
;; file, and it must refuse rather than guess when the thing it would patch is
;; not a literal.
(require rackunit racket/list racket/string racket/file racket/path racket/system
         racket/port racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/emit-racket glide-pptx/emit-rhombus
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
  (define program (build-path dir "deck.rkt"))
  (write-racket-deck d program #:source-name (path->string pptx))
  (define exported (build-path dir "deck.pptx"))
  (define picts (parameterize ([current-media-base dir])
                  (dynamic-require `(file ,(path->string program)) 'all-slides)))
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
  (check-true (regexp-match? #rx"57[.]6 158[.]4" (car (first changed))) "was the old position")
  (check-true (regexp-match? #rx"100[.]0 200[.]0" (cdr (first changed))) "is the new one")
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
  (define program (build-path dir "deck.rkt"))
  (call-with-output-file program #:exists 'replace
    (lambda (o)
      (write-string (string-join
                     '("#lang racket/base"
                       "(require pict glide-pptx/runtime)"
                       "(provide all-slides)"
                       "(define left 40.0)"
                       "(define slide-1"
                       "  (slide-canvas"
                       "   #:width 480.0 #:height 270.0 #:background (hex \"FFFFFF\")"
                       "   (at left 60.0 #:tag \"Box\""
                       "       (shape-pict #:width 100.0 #:height 40.0"
                       "                   #:fill (hex \"4472C4\")))))"
                       "(define all-slides (list slide-1))"
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
  (check-true (regexp-match? #rx"#:nudge [(]list 160[.]0 40[.]0[)]" after1)
              (format "a correction of the right size was recorded:\n~a" after1))
  (check-equal? (length (regexp-match* #rx"#:nudge" after1)) 1 "exactly one correction")
  (check-true (regexp-match? #rx"at left 60[.]0" after1)
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
  (check-equal? (length (regexp-match* #rx"#:nudge" after2)) 1
                (format "still exactly one correction, not a stack of them:\n~a" after2))
  (check-true (regexp-match? #rx"#:nudge [(]list 220[.]0 10[.]0[)]" after2)
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
  (define picts (parameterize ([current-media-base dir])
                  (dynamic-require `(file ,(path->string program)) 'all_slides)))
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
               #:when (regexp-match? #rx"#:tag \"Title 1\"" l))
      l))
  (define b (title-lines before))
  (define a (title-lines (file->string program)))
  (check-equal? (length a) 3)
  (check-equal? (first a) (first b) "slide 1's title is untouched")
  (check-equal? (second a) (second b) "slide 2's title is untouched")
  (check-not-equal? (third a) (third b) "slide 3's title moved")
  (check-true (and (regexp-match? #rx"111" (third a)) (regexp-match? #rx"222" (third a)))
              "to where it was dragged"))

;; ------------------------------------------ what code shapes can be synced

;; The floor: an element an edit cannot be traced back to is refused, not
;; guessed at. One `at` in a loop draws several elements under one tag, and
;; keying on that tag would land a drag on an arbitrary one of them.
(let ()
  (define dir (build-path work "loop"))
  (make-directory* dir)
  (define (write-loop! path tag-expr)
    (display-to-file
     (string-append
      "#lang racket/base\n(require pict glide-pptx/runtime)\n"
      "(provide all-slides slide-1)\n"
      "(define slide-1\n"
      "  (slide-canvas #:width 720.0 #:height 540.0 #:background (hex \"FFFFFF\")\n"
      "    (for/list ([i (in-range 3)])\n"
      "      (at (+ 20.0 (* i 120.0)) 60.0 #:tag " tag-expr "\n"
      "          (shape-pict #:width 100.0 #:height 60.0 #:shape \"rect\"\n"
      "                      #:fill (hex \"4472C4\"))))))\n"
      "(define all-slides (list slide-1))\n")
     path #:exists 'replace))

  ;; `for/list` hands `slide-canvas` a list, which it splices -- so this is a
  ;; program shape that runs, and the refusal below is about the tags, not the
  ;; loop.
  (define same (build-path dir "same.rkt"))
  (write-loop! same "\"Box\"")
  (define same-deck (build-path dir "same.pptx"))
  (define picts (program-picts-fresh same))
  (check-equal? (length picts) 1 "the loop program builds one slide")
  (picts->pptx picts same-deck #:width 720.0 #:height 540.0)
  (check-exn #rx"appears 3 times"
             (lambda () (sync-once same same-deck #:workdir (build-path dir "w1")))
             "three elements under one tag is refused")

  ;; With a distinct tag each, the same loop syncs. The tag is computed, so a
  ;; drag has no literal to rewrite -- which is reported, not silently dropped.
  (define distinct (build-path dir "distinct.rkt"))
  (write-loop! distinct "(format \"Box ~a\" i)")
  (define distinct-deck (build-path dir "distinct.pptx"))
  (picts->pptx (program-picts-fresh distinct) distinct-deck #:width 720.0 #:height 540.0)
  (define r0 (sync-once distinct distinct-deck #:workdir (build-path dir "w2")))
  (check-true (sync-report-base-written? r0) "distinct tags record a base")
  (check-equal? (sync-report-actions r0) '() "with nothing to merge")

  (check-true (drag-in-deck! distinct-deck 1 "Box 1" 300.0 300.0))
  (define r (sync-once distinct distinct-deck #:workdir (build-path dir "w2")))
  (check-equal? (length (sync-report-actions r)) 1 "the drag is seen")
  (check-equal? (sync-report-applied r) '() "but cannot be applied")
  (check-equal? (length (sync-report-skipped r)) 1 "and says so")
  (check-regexp-match #rx"tagged `at` form" (cdr (first (sync-report-skipped r)))))
