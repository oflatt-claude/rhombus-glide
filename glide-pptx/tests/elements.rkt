#lang racket/base
;; Every element on a page of its own, compared against LibreOffice.
;;
;; A whole-slide pixel diff cannot see a small thing that is wrong. Ten missing
;; arrowheads on a busy slide moved its mean error from 2.15% to 2.16%, which is
;; noise -- so the arrows were drawn as plain lines for as long as this existed.
;;
;; Two changes fix that. Each element gets a page the size of its own box, so it
;; is the whole picture rather than a detail of one. And the error is measured
;; against the *ink* rather than the page, because a thin line on its own page
;; is still mostly white: the same missing arrowheads score 9.6% against 17.6%
;; that way, where against the page they scored 2.02% against 2.46%.
(require rackunit racket/list racket/file racket/path racket/format racket/string
         racket/class racket/draw racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/export glide-pptx/verify)

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-elements"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

(define MARGIN 12.0)
;; What is left at the top of the list is text: a title's glyphs all shift
;; together when the baseline is a point out, so a third of its ink can differ
;; while the words are right. Shapes sit far below that, which is what this is
;; for -- it found an arrowhead that was never drawn, and an arrow whose head was
;; half the shape instead of a third, both invisible in a whole-slide diff.
(define INK-LIMIT 0.35)

;; The share of the drawing that differs: pixels that disagree, over pixels
;; either side inked.
;;
;; Sampled on a common grid rather than compared pixel for pixel, because the two
;; renderers do not agree on the page's pixel size -- 1205x836 against 1201x832
;; for the same element. Four pixels of difference shifts everything, and against
;; a thin outline that reads as almost every pixel being wrong.
(define GRID 700)
;; Ink is thin: an outline is two or three pixels wide, so a line that is a pixel
;; off matches nothing at all when compared point for point. What is being asked
;; is whether the drawing is *there*, not whether it is aligned to the pixel, so
;; ink counts as matched when the other side has ink nearby.
(define SLACK 3)

(define (sampler path)
  (define bm (read-bitmap path))
  (define w (send bm get-width)) (define h (send bm get-height))
  (define px (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h px)
  ;; Sampled on a common grid, because the two renderers do not agree on the
  ;; page's pixel size -- 1205x836 against 1201x832 for the same element.
  (define g (make-vector (* GRID GRID) 255))
  (for* ([j (in-range GRID)] [i (in-range GRID)])
    (define x (min (sub1 w) (inexact->exact (floor (* (/ (+ i 0.5) GRID) w)))))
    (define y (min (sub1 h) (inexact->exact (floor (* (/ (+ j 0.5) GRID) h)))))
    (vector-set! g (+ i (* j GRID)) (bytes-ref px (+ 1 (* 4 (+ x (* y w)))))))
  g)

(define (near-ink? g i j)
  (for*/or ([dj (in-range (- SLACK) (add1 SLACK))]
            [di (in-range (- SLACK) (add1 SLACK))])
    (define x (+ i di)) (define y (+ j dj))
    (and (< -1 x GRID) (< -1 y GRID)
         (< (vector-ref g (+ x (* y GRID))) 200))))

(define (ink-error a-path b-path)
  (define a (sampler a-path))
  (define b (sampler b-path))
  (define-values (diff ink)
    (for*/fold ([d 0] [k 0]) ([j (in-range GRID)] [i (in-range GRID)])
      (define ai (< (vector-ref a (+ i (* j GRID))) 200))
      (define bi (< (vector-ref b (+ i (* j GRID))) 200))
      (cond
        [(and (not ai) (not bi)) (values d k)]
        ;; Ink on one side with none near it on the other is a real difference:
        ;; something drawn that should not be, or not drawn that should be.
        [(and ai (not (near-ink? b i j))) (values (add1 d) (add1 k))]
        [(and bi (not (near-ink? a i j))) (values (add1 d) (add1 k))]
        [else (values d (add1 k))])))
  (if (zero? ink) 0.0 (/ (exact->inexact diff) ink)))

;; One element, alone, on a page the size of its own box.
(define (element-deck src-deck e)
  (define b (element-bbox e))
  (define w (+ (* 2 MARGIN) (max 8.0 (bbox-w b))))
  (define h (+ (* 2 MARGIN) (max 8.0 (bbox-h b))))
  (define moved
    (element-map-bbox e (lambda (bb)
                          (make-bbox (+ MARGIN (- (bbox-x bb) (bbox-x b)))
                                     (+ MARGIN (- (bbox-y bb) (bbox-y b)))
                                     (bbox-w bb) (bbox-h bb)
                                     #:rot (bbox-rot bb)
                                     #:flip-h? (bbox-flip-h? bb)
                                     #:flip-v? (bbox-flip-v? bb)))))
  (values (deck w h (list (slide 1 "one" w h #f '() (list moved)))
                (deck-media-dir src-deck) (deck-source src-deck))
          w h))

(define (elements-of name)
  (define src (build-path decks-dir (string-append name ".pptx")))
  (define dir (build-path work name))
  (make-directory* dir)
  (define d (pptx->deck src #:workdir (build-path dir "u")))
  (define out '())
  (for* ([s (in-list (deck-slides d))]
         [e (in-list (slide-elements s))])
    (define tag (format "s~a-~a" (slide-index s)
                        (regexp-replace* #px"[^A-Za-z0-9]+" (element-name e) "_")))
    (with-handlers ([exn:fail? (lambda (ex)
                                 (set! out (cons (list tag 'failed (exn-message ex)) out)))])
      (define-values (d1 w h) (element-deck d e))
      (define pptx (build-path dir (format "~a.pptx" tag)))
      (picts->pptx (deck->picts d1) pptx #:width w #:height h)
      ;; Rendered so the element is about this many pixels across, whatever its
      ;; size on the slide. A 40pt shape at a fixed 200 dpi is a hundred pixels
      ;; of which the edge is most, and antialiasing then swamps the measure --
      ;; the same element scored 65% small and 10% blown up.
      (define dpi (max 96.0 (min 600.0 (/ (* 1200.0 72.0) (max w h)))))
      (define r (verify-deck pptx (build-path dir tag) #:dpi dpi))
      (define pages (and r (deck-diff-pages r)))
      (when (and pages (pair? pages))
        (define sub (build-path dir tag (format "~a" tag)))
        (define ours (build-path sub "our-page-1.png"))
        (define ref (build-path sub "ref-page-1.png"))
        (when (and (file-exists? ours) (file-exists? ref))
          (set! out (cons (list tag (ink-error ours ref) #f) out))))))
  (reverse out))

(define all
  (append* (for/list ([name (in-list '("01-placeholders" "02-text" "03-shapes"
                                       "04-pictures-groups" "05-realistic"))])
             (elements-of name))))

(define compared (filter (lambda (r) (real? (second r))) all))
(define ranked (sort compared > #:key second))
(printf "~a elements compared, worst first:\n" (length ranked))
(for ([r (in-list (take ranked (min 8 (length ranked))))])
  (printf "  ~a%  ~a\n" (~r (* 100 (second r)) #:precision 1) (first r)))

(for ([r (in-list all)])
  (check-true (real? (second r))
              (format "~a could not be compared: ~a" (first r) (third r))))
(for ([r (in-list ranked)])
  (check-true (< (second r) INK-LIMIT)
              (format "~a: ~a% of its drawing differs from LibreOffice's"
                      (first r) (~r (* 100 (second r)) #:precision 1))))

(printf "element tests done; artifacts under ~a\n" work)
