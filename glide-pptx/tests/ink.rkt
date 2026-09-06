#lang racket/base
;; How much of a drawing differs from another drawing of the same thing.
;;
;; Not a pixel diff. Two renderers never agree on the pixel: they disagree on
;; the page's pixel size -- 1205x836 against 1201x832 for one element -- and
;; they place and rasterize a glyph slightly differently. Compared point for
;; point, a correct outline that sits half a pixel over reads as almost every
;; pixel being wrong, and a number like that can only be given a loose bound,
;; which is a bound that catches nothing.
;;
;; So two things. The comparison is against the *ink* rather than the page: a
;; thin line on its own page is mostly white, and dividing by the page hides it.
;; And ink counts as matched when the other side has ink nearby, where "nearby"
;; is a distance on the page rather than a number of cells -- both images are
;; sampled onto a grid of square cells, so the slack is the same sideways as it
;; is downwards. It was not, before: a line of text sampled onto a square grid
;; was squashed four to one, and three cells of slack meant five pixels across
;; and one down, so anything a pixel low read as entirely wrong.
;;
;; What is left after that is what this is for: something drawn that should not
;; be, or not drawn that should be.
(require racket/class racket/draw racket/math)
(provide ink-error ink-error/slack GRID-LONG SLACK)

;; The long side of the sampling grid; the short side keeps the aspect, so a
;; cell is square.
(define GRID-LONG 700)
;; Cells, and so pixels of the long side over GRID-LONG. Three of them at this
;; grid is about half a percent of the element's longer side.
(define SLACK 3)

(define (grid-for w h)
  (define k (/ (exact->inexact GRID-LONG) (max w h)))
  (values (max 1 (inexact->exact (round (* k w))))
          (max 1 (inexact->exact (round (* k h))))))

;; The darkest pixel in each cell, 0..255.
;;
;; Darkest rather than one of them, and rather than their average. A cell can
;; cover two pixels or ten, and ink is thin -- an outline is a pixel or two
;; across. Picking one pixel per cell means a line is hit in one image and
;; missed in the other whenever the two sit half a pixel apart, which is a
;; difference the metric invented: measured against itself, the same drawing
;; moved half a pixel downwards read as 45% wrong. Averaging instead dilutes a
;; thin line until it stops counting as ink at all. The darkest pixel asks
;; what the cell is for -- whether anything was drawn in it -- and answers the
;; same either side of half a pixel.
;; Read once and handed on. A comparison used to open each file for its size
;; and again for its pixels, and a few dozen elements of that was enough for
;; the drawing library to fall over with an invalid memory reference.
(define (load-pixels path)
  (define bm (read-bitmap path))
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define px (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h px)
  (values px w h))

(define (sample px w h gw gh)
  (define g (make-vector (* gw gh) 255))
  (for* ([j (in-range gh)] [i (in-range gw)])
    (define x0 (inexact->exact (floor (* (/ (exact->inexact i) gw) w))))
    (define x1 (max (add1 x0) (inexact->exact (ceiling (* (/ (+ i 1.0) gw) w)))))
    (define y0 (inexact->exact (floor (* (/ (exact->inexact j) gh) h))))
    (define y1 (max (add1 y0) (inexact->exact (ceiling (* (/ (+ j 1.0) gh) h)))))
    (define darkest
      (for*/fold ([m 255]) ([y (in-range y0 (min h y1))] [x (in-range x0 (min w x1))])
        (min m (bytes-ref px (+ 1 (* 4 (+ x (* y w))))))))
    (vector-set! g (+ i (* j gw)) darkest))
  g)

(define (ink-error a-path b-path)
  (ink-error/slack a-path b-path SLACK))

(define (ink-error/slack a-path b-path slack)
  (define-values (apx aw ah) (load-pixels a-path))
  (define-values (bpx bw bh) (load-pixels b-path))
  (define-values (gw gh) (grid-for aw ah))
  (define a (sample apx aw ah gw gh))
  (define b (sample bpx bw bh gw gh))
  (define (inked? g i j) (< (vector-ref g (+ i (* j gw))) 200))
  (define (near-ink? g i j)
    (for*/or ([dj (in-range (- slack) (add1 slack))]
              [di (in-range (- slack) (add1 slack))])
      (define x (+ i di))
      (define y (+ j dj))
      (and (< -1 x gw) (< -1 y gh) (inked? g x y))))
  (define-values (diff ink)
    (for*/fold ([d 0] [k 0]) ([j (in-range gh)] [i (in-range gw)])
      (define ai (inked? a i j))
      (define bi (inked? b i j))
      (cond
        [(and (not ai) (not bi)) (values d k)]
        [(and ai (not (near-ink? b i j))) (values (add1 d) (add1 k))]
        [(and bi (not (near-ink? a i j))) (values (add1 d) (add1 k))]
        [else (values d (add1 k))])))
  (if (zero? ink) 0.0 (/ (exact->inexact diff) ink)))
