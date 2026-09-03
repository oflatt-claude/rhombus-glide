#lang racket/base
;; The whole pipeline over a corpus of real decks.
;;
;; Five fixtures written by us test what we thought to test. LibreOffice's pptx
;; regression suite -- several hundred decks, each aimed at one awkward corner of
;; the format -- tests what we did not, and it found real bugs on the first run:
;; a custom path that opens with a line instead of a move, and a picture whose
;; relationship resolves to nothing.
;;
;; What is checked here is *crash-freedom* across import, render, draw, export
;; and re-import, not fidelity. These files are deliberately strange, and a
;; per-deck pixel budget for them would mean nothing. The approximation notes are
;; printed as an inventory, which is how the unsupported-feature list stays
;; honest.
;;
;; The decks are not committed -- they belong to LibreOffice. Run
;; `tools/fetch-corpus.sh` to get them; with no corpus present this test says so
;; and passes.
(require rackunit racket/list racket/string racket/file racket/path racket/format
         racket/class racket/draw racket/runtime-path pict
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/export)

(define-runtime-path corpus-dir "corpus")

(define decks
  (if (directory-exists? corpus-dir)
      (sort (filter (lambda (p) (regexp-match? #rx"[.]pptx$" (path->string p)))
                    (directory-list corpus-dir))
            string<? #:key path->string)
      '()))

(cond
  [(null? decks)
   (printf "no corpus present; run tools/fetch-corpus.sh to fetch one\n")]
  [else
   ;; These decks include 62 with charts or SmartArt. Elsewhere that is an error,
  ;; because an empty box would quietly replace the content on a round trip.
  ;; Here nothing is round-tripped -- the question is only whether we crash.
  (current-allow-unsupported? #t)
  (define work (build-path (find-system-path 'temp-dir) "glide-pptx-corpus"))
   (delete-directory/files work #:must-exist? #f)
   (make-directory* work)

   (define failures '())
   (define notes (make-hash))

   (for ([d (in-list decks)] [i (in-naturals 1)])
     (define name (path->string d))
     (define warns (box '()))
     (define (phase label thunk)
       (with-handlers ([(lambda (_e) #t)
                        (lambda (e)
                          (set! failures
                                (cons (list name label
                                            (if (exn? e)
                                                (first (string-split (exn-message e) "\n"))
                                                (format "~a" e)))
                                      failures))
                          #f)])
         (thunk)))
     (define deck
       (parameterize ([current-warnings warns])
         (phase 'import (lambda () (pptx->deck (build-path corpus-dir d)
                                               #:workdir (build-path work (format "u~a" i)))))))
     (define picts
       (and deck (parameterize ([runtime-warnings warns])
                   (phase 'render (lambda () (deck->picts deck))))))
     (when (and picts (pair? picts))
       ;; Actually draw a page: dc-level bugs only show up when something draws.
       (parameterize ([runtime-warnings warns])
         (phase 'draw
                (lambda ()
                  (define p (first picts))
                  (define (dim v) (max 1 (min 400 (inexact->exact (ceiling v)))))
                  (define bm (make-bitmap (dim (pict-width p)) (dim (pict-height p))))
                  (draw-pict p (new bitmap-dc% [bitmap bm]) 0 0)
                  #t)))
       (define out (build-path work (format "out~a.pptx" i)))
       (define wrote
         (parameterize ([current-export-warnings warns])
           (phase 'export (lambda () (picts->pptx picts out
                                                  #:width (deck-width deck)
                                                  #:height (deck-height deck))))))
       (when wrote
         (phase 'reimport
                (lambda () (pptx->deck out #:workdir (build-path work (format "r~a" i)))))))
     ;; Note kinds, with names and numbers folded out so they group.
     (for ([w (in-list (remove-duplicates (unbox warns)))])
       (hash-update! notes (regexp-replace* #px"[0-9]+|\"[^\"]*\"" w "_") add1 0)))

   (define broken (remove-duplicates (map first failures)))
   (printf "~a decks, ~a with a failure\n" (length decks) (length broken))
   (for ([f (in-list (reverse failures))])
     (printf "  ~a  [~a]  ~a\n" (~a (first f) #:min-width 32) (second f) (third f)))
   (printf "\napproximation notes, by kind:\n")
   (for ([p (in-list (sort (hash->list notes) > #:key cdr))])
     (printf "  ~a  ~a\n" (~a (cdr p) #:min-width 5) (car p)))

   (check-equal? failures '()
                 (format "~a of ~a decks failed somewhere in the pipeline"
                         (length broken) (length decks)))
   (delete-directory/files work #:must-exist? #f)])

(printf "corpus tests done\n")
