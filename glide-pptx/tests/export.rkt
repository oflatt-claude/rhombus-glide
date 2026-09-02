#lang racket/base
;; Does an exported .pptx look like the picts it came from?
;;
;; The measurement is the same one used on the import side: render our picts to
;; PDF, have LibreOffice render the exported .pptx, rasterize both with the same
;; rasterizer and compare. Anything the writer gets wrong shows up here.
;;
;; The fixture decks are used as a convenient source of realistic picts; this
;; test is about the *export* path, so the reference is our own render rather
;; than LibreOffice's reading of the original deck.
(require rackunit racket/list racket/file racket/path racket/format
         racket/runtime-path pict
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/export glide-pptx/draw-ir glide-pptx/record-adapt glide-pptx/verify
         glide-pptx/emit-racket
         (only-in file/unzip read-zip-directory zip-directory-entries unzip
                  make-filesystem-entry-reader))

(define-runtime-path decks-dir "decks")

;; `zip-directory-entries` yields entry names as bytes.
(define (slide-part pptx n)
  (define dir (make-temporary-file "slidepart~a" 'directory))
  (call-with-input-file pptx (lambda (in) (parameterize ([current-directory dir])
                                            (unzip in (make-filesystem-entry-reader)))))
  (define xml (file->string (build-path dir "ppt" "slides" (format "slide~a.xml" n))))
  (delete-directory/files dir #:must-exist? #f)
  xml)

(define (zip-entry-names path)
  (for/list ([e (in-list (zip-directory-entries (read-zip-directory path)))])
    (bytes->string/utf-8 e)))

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-export"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; Compared against *our* render, so these budgets are looser than the semantic
;; ones further down, and deliberately so: an exported preset is drawn by
;; PowerPoint's own geometry, which is not the approximation `geometry.rkt`
;; draws. That gap is our renderer being approximate, not the writer being
;; wrong, which is why the meaningful check is against the original deck.
(define budgets
  (hash "01-placeholders"    '(0.015 . 0.020)
        "02-text"            '(0.012 . 0.020)
        "03-shapes"          '(0.020 . 0.035)
        "04-pictures-groups" '(0.008 . 0.015)
        "05-realistic"       '(0.020 . 0.030)))

(define (export-fidelity name)
  (define dir (build-path work name))
  (make-directory* dir)
  (define pptx (build-path decks-dir (string-append name ".pptx")))
  (define warnings (box '()))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define picts (deck->picts d))
  ;; Our own render is the reference.
  (define ours (build-path dir "ours.pdf"))
  (picts->pdf picts ours #:width (deck-width d) #:height (deck-height d))
  ;; The exported package, as LibreOffice reads it.
  (define exported (build-path dir "exported.pptx"))
  (parameterize ([current-export-warnings warnings])
    (picts->pptx picts exported #:width (deck-width d) #:height (deck-height d)))
  (check-true (file-exists? exported) (format "~a: a package was written" name))
  (define ref (libreoffice-pdf exported (build-path dir "ref")))
  (define a (rasterize-pdf ours (build-path dir "ours") #:dpi 96))
  (define b (rasterize-pdf ref (build-path dir "theirs") #:dpi 96))
  (check-equal? (length b) (length a) (format "~a: page count survives the round trip" name))
  (values a b (remove-duplicates (reverse (unbox warnings)))))

(for ([name (in-list (sort (hash-keys budgets) string<?))])
  (define budget (hash-ref budgets name))
  (define-values (ours theirs warnings) (export-fidelity name))
  (printf "~a\n" name)
  (for ([a (in-list ours)] [b (in-list theirs)] [i (in-naturals 1)])
    (define-values (mae bad w h) (compare-images a b))
    (printf "  page ~a  mean err ~a%  pixels off ~a%\n" i
            (~r (* 100 mae) #:precision '(= 2)) (~r (* 100 bad) #:precision '(= 2)))
    (check-true (<= mae (car budget))
                (format "~a page ~a: mean error ~a% within ~a%" name i
                        (~r (* 100 mae) #:precision 2) (* 100 (car budget))))
    (check-true (<= bad (cdr budget))
                (format "~a page ~a: ~a% of pixels off, budget ~a%" name i
                        (~r (* 100 bad) #:precision 2) (* 100 (cdr budget)))))
  (for ([w (in-list warnings)]) (printf "  ! ~a\n" w)))

;; The writer should be producing real PowerPoint objects, not rasterizing. If a
;; simple slide ever comes out as pictures, something regressed badly.
(let ()
  (define p (vc-append 10
                       (filled-rectangle 100 40 #:color "SteelBlue")
                       (filled-ellipse 80 40 #:color "orange")))
  (define page (pict->display-page (lambda (dc) (draw-pict p dc 0 0)) 200.0 120.0))
  (define items (display-page-items page))
  (check-equal? (length items) 2 "two shapes, two items")
  (check-true (it:rect? (first items)) "a filled-rectangle stays a rectangle")
  (check-true (it:ellipse? (second items)) "a filled-ellipse stays an ellipse")
  (check-false (ormap it:image? items) "nothing was rasterized"))

;; A deck with images has to come out with media parts in the package. This
;; guards a real bug: a generated program resolves its images relative to
;; itself, which it cannot work out when loaded with `dynamic-require`, so the
;; pictures silently vanished from the export.
(let ()
  (define dir (build-path work "media-check"))
  (make-directory* dir)
  (define pptx (build-path decks-dir "04-pictures-groups.pptx"))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define program (build-path dir "deck.rkt"))
  (write-racket-deck d program #:source-name (path->string pptx))
  (define picts
    (parameterize ([current-media-base dir])
      (dynamic-require `(file ,(path->string program)) 'all-slides)))
  (define out (build-path dir "out.pptx"))
  (picts->pptx picts out #:width (deck-width d) #:height (deck-height d))
  (define media
    (filter (lambda (n) (regexp-match? #rx"^ppt/media/" n)) (zip-entry-names out)))
  (check-true (>= (length media) 2)
              (format "the exported package carries its images, found ~a" media)))

;; The strong check on the semantic path: a deck exported from a generated
;; program should reproduce the *original* deck, because it hands PowerPoint back
;; the same presets and text bodies it had. This scores better than our own
;; renderer does against the same original, which is the whole point of knowing
;; an element's structure rather than only its ink.
(define semantic-budgets
  (hash "01-placeholders" '(0.006 . 0.012)
        "02-text"         '(0.010 . 0.020)
        "03-shapes"       '(0.010 . 0.020)
        "04-pictures-groups" '(0.006 . 0.010)
        "05-realistic"    '(0.006 . 0.012)))

(for ([name (in-list (sort (hash-keys semantic-budgets) string<?))])
  (define budget (hash-ref semantic-budgets name))
  (define dir (build-path work (string-append name "-semantic")))
  (make-directory* dir)
  (define pptx (build-path decks-dir (string-append name ".pptx")))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  ;; Go through the emitted program, so this exercises what a user would run.
  (define program (build-path dir "deck.rkt"))
  (write-racket-deck d program #:source-name (path->string pptx))
  (define picts
    (parameterize ([current-media-base dir])
      (dynamic-require `(file ,(path->string program)) 'all-slides)))
  (for ([p (in-list picts)])
    (check-true (semantic-page? p)
                (format "~a: a generated slide carries its structure" name)))
  (define exported (build-path dir "exported.pptx"))
  (picts->pptx picts exported #:width (deck-width d) #:height (deck-height d))
  (define theirs (rasterize-pdf (libreoffice-pdf exported (build-path dir "ref"))
                                (build-path dir "theirs") #:dpi 96))
  (define original (rasterize-pdf (libreoffice-pdf pptx (build-path dir "orig"))
                                  (build-path dir "orig-page") #:dpi 96))
  (printf "~a (semantic, vs the original deck)\n" name)
  (for ([a (in-list original)] [b (in-list theirs)] [i (in-naturals 1)])
    (define-values (mae bad w h) (compare-images a b))
    (printf "  page ~a  mean err ~a%  pixels off ~a%\n" i
            (~r (* 100 mae) #:precision '(= 2)) (~r (* 100 bad) #:precision '(= 2)))
    (check-true (<= mae (car budget))
                (format "~a page ~a: ~a% within ~a%" name i
                        (~r (* 100 mae) #:precision 2) (* 100 (car budget))))
    (check-true (<= bad (cdr budget))
                (format "~a page ~a: ~a% of pixels off, budget ~a%" name i
                        (~r (* 100 bad) #:precision 2) (* 100 (cdr budget))))))

;; Structure has to survive into the package: a generated deck of preset shapes
;; and paragraphs must not come out as freeform paths and one box per word.
(let ()
  (define dir (build-path work "structure"))
  (make-directory* dir)
  (define pptx (build-path decks-dir "05-realistic.pptx"))
  (define d (pptx->deck pptx #:workdir (build-path dir "unpacked")))
  (define out (build-path dir "out.pptx"))
  (picts->pptx (deck->picts d) out #:width (deck-width d) #:height (deck-height d))
  (define xml (slide-part out 3))
  (check-true (regexp-match? #rx"prst=\"roundRect\"" xml) "a roundRect stays a roundRect")
  (check-true (regexp-match? #rx"prst=\"rightArrow\"" xml) "an arrow stays an arrow")
  (check-false (regexp-match? #rx"<a:custGeom" xml) "no shape fell back to a path")
  (check-true (regexp-match? #rx"descr=\"glide-pptx:" xml) "elements carry their tags")
  (check-true (>= (length (regexp-match* #rx"<a:t>" xml)) 5)
              "text is text, in runs"))

(printf "export tests done; artifacts under ~a\n" work)
