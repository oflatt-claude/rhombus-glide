#lang racket/base
;; A display list: the normalized drawing model that backends consume.
;;
;; This is deliberately a model of *drawing*, not of PowerPoint. SVG and canvas
;; can express arbitrary clipping and shear where DrawingML cannot, so trimming
;; the IR to what pptx supports would hand every other backend a limitation it
;; does not have. Each backend degrades on its own terms.
;;
;; Coordinates are in PostScript points with the origin at the page's top-left,
;; with any drawing transform already applied. Rotation is degrees clockwise
;; about an item's own center, which is the one form DrawingML can express.
(require racket/list racket/math (prefix-in ir: "ir.rkt"))
(provide (all-defined-out))

;; ------------------------------------------------------------------- paint

;; Components are 0..255, alpha 0.0..1.0.
(struct rgba* (r g b a) #:prefab)

;; stops are (list fraction rgba*), ordered.
(struct fill:solid (color) #:prefab)
(struct fill:linear (x0 y0 x1 y1 stops) #:prefab)
(struct fill:radial (x0 y0 r0 x1 y1 r1 stops) #:prefab)

;; dash is 'solid, 'dash, 'dot or 'dash-dot; cap is 'flat, 'round or 'square;
;; join is 'miter, 'round or 'bevel. A #f pen means nothing is stroked.
(struct pen* (color width dash cap join) #:prefab)

;; ------------------------------------------------------------------ paths

(struct seg:move (x y) #:prefab)
(struct seg:line (x y) #:prefab)
(struct seg:curve (x1 y1 x2 y2 x y) #:prefab)
(struct seg:close () #:prefab)

;; -------------------------------------------------------------------- items

;; `radius` is the corner radius in points; 0 for a plain rectangle.
(struct it:rect (x y w h rot radius fill pen) #:prefab)
(struct it:ellipse (x y w h rot fill pen) #:prefab)
;; Coordinates in `segs` are absolute and already transformed, so a path can
;; carry a shear that no other item can.
(struct it:path (segs fill pen) #:prefab)
;; The box is where the text's own bounding box lands; `ascent` is how far the
;; first baseline sits below the box top, which is what lets another renderer
;; place the same glyphs.
(struct it:text (x y w h rot str face size bold? italic? underline? ascent color)
  #:prefab)
;; `argb` is raw ARGB rows of `src-w` by `src-h`. `tag` is set when the image
;; stands in for a whole element -- a subtree with no structure of its own,
;; flattened so the editor shows one object instead of a pile of pieces.
(struct it:image (x y w h rot argb src-w src-h tag) #:prefab)

;; --------------------------------------------------- items with known structure

;; These carry more than a drawing: a shape's preset name rather than the path
;; it flattens to, and a whole text body rather than one box per drawn word. A
;; backend that can use them produces something editable -- a real PowerPoint
;; shape with reflowable text, or an HTML element with selectable text -- and one
;; that cannot is free to ignore them and use the flattened items instead.
;;
;; `body` is an `ir:text-body`; `tag` is the element's name, or #f.
(struct it:preset (x y w h rot name adjust flip-h? flip-v? fill pen body tag) #:prefab)
(struct it:textbox (x y w h rot body tag) #:prefab)
;; A path whose geometry came from a known shape, so it keeps its text and tag.
(struct it:shape-path (segs fill pen box body tag) #:prefab)
;; `src` is a file path; the writer reads and embeds it.
(struct it:picture (x y w h rot src crop flip-h? flip-v? pen opacity tag) #:prefab)
;; A group, kept as one. Dissolving it into its children would lose the
;; grouping the author made in the editor -- and anything attached to the group,
;; an appear animation most of all, would have nothing left to attach to.
(struct it:group (x y w h rot items tag) #:prefab)

;; An item that stands for one element of a slide, and so takes part in a sync.
;; A flattened element counts: it is a picture rather than a shape, but it is
;; still one object with one identity, and dragging it has somewhere to land.
(define (semantic-item? i)
  (or (it:preset? i) (it:textbox? i) (it:shape-path? i) (it:picture? i)
      (it:group? i)
      (and (it:image? i) (it:image-tag i) #t)))

;; The element name an item carries, or #f.
(define (item-tag i)
  (cond
    [(it:preset? i) (it:preset-tag i)]
    [(it:textbox? i) (it:textbox-tag i)]
    [(it:shape-path? i) (it:shape-path-tag i)]
    [(it:picture? i) (it:picture-tag i)]
    [(it:image? i) (it:image-tag i)]
    [(it:group? i) (it:group-tag i)]
    [else #f]))

;; A page of items in paint order, plus the size it was drawn at. `background`
;; is a fill for the whole page, kept apart from the items because a slide
;; background is not a shape -- treating it as one makes it look like an extra
;; element to anything comparing two decks.
(struct display-page (width height background items) #:prefab)

(define (item-box i)
  (cond
    [(it:rect? i) (list (it:rect-x i) (it:rect-y i) (it:rect-w i) (it:rect-h i))]
    [(it:ellipse? i) (list (it:ellipse-x i) (it:ellipse-y i) (it:ellipse-w i) (it:ellipse-h i))]
    [(it:text? i) (list (it:text-x i) (it:text-y i) (it:text-w i) (it:text-h i))]
    [(it:image? i) (list (it:image-x i) (it:image-y i) (it:image-w i) (it:image-h i))]
    [(it:path? i) (segs-bounds (it:path-segs i))]
    [else (list 0.0 0.0 0.0 0.0)]))

(define (segs-bounds segs)
  (define xs '()) (define ys '())
  (define (note! x y) (set! xs (cons x xs)) (set! ys (cons y ys)))
  (for ([s (in-list segs)])
    (cond
      [(seg:move? s) (note! (seg:move-x s) (seg:move-y s))]
      [(seg:line? s) (note! (seg:line-x s) (seg:line-y s))]
      [(seg:curve? s)
       ;; The control points bound the curve, which is enough for a box.
       (note! (seg:curve-x1 s) (seg:curve-y1 s))
       (note! (seg:curve-x2 s) (seg:curve-y2 s))
       (note! (seg:curve-x s) (seg:curve-y s))]
      [else (void)]))
  (if (null? xs)
      (list 0.0 0.0 0.0 0.0)
      (list (apply min xs) (apply min ys)
            (- (apply max xs) (apply min xs))
            (- (apply max ys) (apply min ys)))))

;; ---------------------------------------------------------------- matrices

;; A matrix is (vector a b c d e f), meaning
;;   x' = a*x + c*y + e      y' = b*x + d*y + f
(define identity-matrix (vector 1.0 0.0 0.0 1.0 0.0 0.0))

;; Applies `n` first, then `m`.
(define (mat* m n)
  (define-values (ma mb mc md me mf) (mat-values m))
  (define-values (na nb nc nd ne nf) (mat-values n))
  (vector (+ (* ma na) (* mc nb))
          (+ (* mb na) (* md nb))
          (+ (* ma nc) (* mc nd))
          (+ (* mb nc) (* md nd))
          (+ (* ma ne) (* mc nf) me)
          (+ (* mb ne) (* md nf) mf)))

(define (mat-values m)
  (values (vector-ref m 0) (vector-ref m 1) (vector-ref m 2)
          (vector-ref m 3) (vector-ref m 4) (vector-ref m 5)))

(define (mat-apply m x y)
  (define-values (a b c d e f) (mat-values m))
  (values (+ (* a x) (* c y) e) (+ (* b x) (* d y) f)))

(define (mat-translate dx dy) (vector 1.0 0.0 0.0 1.0 dx dy))
(define (mat-scale sx sy) (vector sx 0.0 0.0 sy 0.0 0.0))
(define (mat-rotate rad)
  ;; racket/draw rotates counterclockwise in a y-down space.
  (vector (cos rad) (- (sin rad)) (sin rad) (cos rad) 0.0 0.0))

;; The uniform scale factor a matrix applies, used for line widths and font
;; sizes, which have no direction of their own.
(define (mat-scale-factor m)
  (define-values (a b c d _e _f) (mat-values m))
  (sqrt (max 0.0 (abs (- (* a d) (* b c))))))

;; How a matrix decomposes, which decides what a backend can express:
;;   (values 'axis sx sy)      no rotation; sx/sy may be negative for a flip
;;   (values 'rotate s deg)    uniform scale with a rotation, in degrees clockwise
;;   (values 'shear #f #f)     neither; the item has to become a path
(define EPS 1e-9)
(define (mat-decompose m)
  (define-values (a b c d _e _f) (mat-values m))
  (cond
    [(and (< (abs b) EPS) (< (abs c) EPS)) (values 'axis a d)]
    [(and (< (abs (- a d)) 1e-7) (< (abs (+ b c)) 1e-7))
     (define s (sqrt (+ (* a a) (* b b))))
     (values 'rotate s (radians->degrees (atan b a)))]
    [else (values 'shear #f #f)]))
