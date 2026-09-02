#lang racket/base
;; Emitted programs must draw exactly what the direct renderer draws.
;;
;; Both go through the same runtime, so any difference means the emitter dropped
;; or garbled something -- a missing keyword, a default assumed wrongly, a
;; coordinate rounded away. Comparing rendered pages catches that where reading
;; the generated source would not.
(require rackunit racket/list racket/file racket/path racket/system racket/port
         racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/emit-racket glide-pptx/emit-rhombus glide-pptx/verify)

(define-runtime-path decks-dir "decks")
(define-runtime-path here ".")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-roundtrip"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

(define racket-exe (find-system-path 'exec-file))

(define (run-program path)
  (define dir (path-only path))
  (define out (open-output-string))
  (define code
    (parameterize ([current-directory dir]
                   [current-output-port out] [current-error-port out])
      (system*/exit-code racket-exe (path->string path))))
  (unless (zero? code)
    (error 'run-program "~a exited with ~a\n~a" path code (get-output-string out)))
  (build-path dir (path-replace-extension (file-name-from-path path) ".pdf")))

(define decks
  (sort (for/list ([f (in-list (directory-list decks-dir))]
                   #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
          f)
        string<? #:key path->string))

(check-true (>= (length decks) 5) "the fixture decks are present")

(for ([deck (in-list decks)])
  (define name (path->string (path-replace-extension deck "")))
  (define pptx (build-path decks-dir deck))
  (define dir (build-path work name))
  (make-directory* dir)

  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))

  ;; The reference for this test is the direct render, not LibreOffice.
  (define direct-pdf (build-path dir "direct.pdf"))
  (picts->pdf (deck->picts d) direct-pdf
              #:width (deck-width d) #:height (deck-height d))
  (define direct-pages (rasterize-pdf direct-pdf (build-path dir "direct") #:dpi 96))

  (for ([lang (in-list '(racket rhombus))])
    (define ext (if (eq? lang 'racket) ".rkt" ".rhm"))
    (define program (build-path dir (string-append name ext)))
    (if (eq? lang 'racket)
        (write-racket-deck d program #:source-name (path->string pptx))
        (write-rhombus-deck d program #:source-name (path->string pptx)))
    (check-true (file-exists? program) (format "~a ~a was emitted" name lang))

    (define pdf (run-program program))
    (define pages (rasterize-pdf pdf (build-path dir (format "~a-page" lang)) #:dpi 96))

    (check-equal? (length pages) (length direct-pages)
                  (format "~a ~a: page count" name lang))
    (for ([a (in-list direct-pages)] [b (in-list pages)] [i (in-naturals 1)])
      (define-values (mae bad w h) (compare-images a b))
      ;; The two paths differ only in how the same runtime calls were spelled,
      ;; so the only allowed difference is antialiasing around coordinates the
      ;; emitter rounded to a thousandth of a point.
      (check-= bad 0.0 0.0005
               (format "~a ~a page ~a: emitted output matches the direct render" name lang i))
      (check-= mae 0.0 0.0005
               (format "~a ~a page ~a: no visible residual difference" name lang i)))))

(printf "roundtrip tests done (~a decks x 2 languages)\n" (length decks))
