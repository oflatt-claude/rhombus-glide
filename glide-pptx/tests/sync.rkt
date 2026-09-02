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
         glide-pptx/sync glide-pptx/sync-state glide-pptx/runtime)

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-sync"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; Moves a tagged shape in a .pptx by rewriting its offset, which is what
;; dragging it in an editor amounts to.
(define (drag! pptx slide tag x y)
  (define dir (make-temporary-file "drag~a" 'directory))
  (define unpacked (build-path dir "u"))
  (make-directory* unpacked)
  (system* (find-executable-path "unzip") "-o" "-q"
           (path->string pptx) "-d" (path->string unpacked))
  (define part (build-path unpacked "ppt" "slides" (format "slide~a.xml" slide)))
  (define d (file->string part))
  (define rx (pregexp (format "(?s:<p:sp>(?:(?!</p:sp>).)*?descr=\"glide-pptx:~a\"(?:(?!</p:sp>).)*?</p:sp>)"
                              (regexp-quote tag))))
  (define m (regexp-match rx d))
  (check-true (and m #t) (format "found the shape tagged ~s to drag" tag))
  (when m
    (define sp (first m))
    (define moved
      (regexp-replace #px"<a:off x=\"\\d+\" y=\"\\d+\"/>" sp
                      (format "<a:off x=\"~a\" y=\"~a\"/>"
                              (inexact->exact (round (* 12700 x)))
                              (inexact->exact (round (* 12700 y))))))
    (call-with-output-file part #:exists 'replace
      (lambda (o) (write-string (string-replace d sp moved) o)))
    (delete-file pptx)
    (parameterize ([current-directory unpacked])
      (apply system* (find-executable-path "zip") "-q" "-r"
             (path->string (path->complete-path pptx))
             (map path->string (directory-list)))))
  (delete-directory/files dir #:must-exist? #f))

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
  (drag! exported 3 "Rounded Rectangle 2" 100.0 200.0)
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

;; ------------------------------------- a computed position is not patchable

(let ()
  (define dir (build-path work "computed"))
  (make-directory* dir)
  (define program (build-path dir "deck.rkt"))
  ;; `left` is a variable, so there is no literal to replace. The merge has to
  ;; say so rather than overwrite a decision the code is making.
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
  (define picts (parameterize ([current-media-base dir])
                  (dynamic-require `(file ,(path->string program)) 'all-slides)))
  (picts->pptx picts exported #:width 480.0 #:height 270.0)
  (sync-once program exported #:workdir (build-path dir "w"))
  (define before (file->string program))
  (drag! exported 1 "Box" 200.0 100.0)
  (define r (sync-once program exported #:workdir (build-path dir "w")))
  (check-equal? (length (filter (lambda (a) (eq? 'moved (sync-action-kind a)))
                                (sync-report-actions r)))
                1 "the move was detected")
  (check-equal? (sync-report-applied r) '() "but not applied")
  (check-equal? (file->string program) before
                "a computed position is left exactly as the author wrote it"))

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
  (drag! exported 3 "Rounded Rectangle 2" 111.0 222.0)
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
