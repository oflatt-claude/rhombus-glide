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
         glide-pptx/emit-rhombus glide-pptx/verify)

(define-runtime-path decks-dir "decks")
(define-runtime-path here ".")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-roundtrip"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

(define racket-exe (find-system-path 'exec-file))

;; Running the program itself opens a slideshow, which is what running a talk
;; should do -- so the PDF comes from its `pdf` submodule.
(define (run-program path)
  (define dir (path-only path))
  (define out (open-output-string))
  (define code
    (parameterize ([current-directory dir]
                   [current-output-port out] [current-error-port out])
      (system*/exit-code racket-exe "-l" "racket/base" "-e"
                         (format "(require (submod (file ~s) pdf))"
                                 (path->string path)))))
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

  (let ([lang 'rhombus])
    (define program (build-path dir (string-append name ".rhm")))
    (write-rhombus-deck d program #:source-name (path->string pptx))
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

(printf "roundtrip tests done (~a decks)\n" (length decks))

;; ------------------------------------------- the program is a slideshow

;; Running a talk should show it. The generated `module main` is a slideshow, so
;; `racket talk.rhm` opens the slides -- it was reported as surprising when it
;; wrote a PDF instead, which is what the PDF's own submodule is now for.
(let ()
  (define dir (build-path work "slideshow"))
  (make-directory* dir)
  (define pptx (build-path decks-dir "05-realistic.pptx"))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define program (build-path dir "show.rhm"))
  (write-rhombus-deck d program #:source-name (path->string pptx))
  (define src (file->string program))

  ;; What `racket show.rhm` runs.
  (check-regexp-match #rx"module main:\n  import: slideshow open" src)
  (check-regexp-match #rx"slide[(]~layout: #'center, Pict.from_handle" src
                      "each slide is handed to slideshow, through the pict bridge")
  (check-regexp-match #rx"module pdf:" src "and the PDF has its own submodule")

  ;; Slideshow needs a display, so the real thing is only checked where there is
  ;; one to borrow. It is the check that matters: the pages come out.
  (define xvfb (find-executable-path "xvfb-run"))
  (cond
    [(not xvfb) (printf "no xvfb-run; skipping the slideshow render\n")]
    [else
     (define out (open-output-string))
     (define code
       (parameterize ([current-directory dir]
                      [current-output-port out] [current-error-port out])
         (system*/exit-code xvfb "-a" racket-exe (path->string program)
                            "--widescreen" "--pdf" "-c" "-e" "-o" "show.pdf")))
     (define pdf (build-path dir "show.pdf"))
     (check-equal? code 0 (format "the slideshow ran:\n~a" (get-output-string out)))
     (check-true (file-exists? pdf) "and wrote its pages")
     (when (file-exists? pdf)
       (check-equal? (length (rasterize-pdf pdf (build-path dir "pg") #:dpi 24))
                     (length (deck-slides d))
                     "one page per slide"))]))
