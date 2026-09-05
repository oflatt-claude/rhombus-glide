#lang racket/base
;; Structure-aware export: turn the descriptors a pict carries into items that
;; keep their meaning, and fall back to the flattened display list for anything
;; that carries none.
;;
;; The walk is over the runtime's own composition -- `slide-canvas` collecting
;; `placed` elements -- rather than over the pict tree, which makes it exact
;; rather than a reconstruction. Elements are emitted in paint order, and an
;; element with no descriptor is rendered on its own and spliced in at the right
;; point, so z-order is preserved either way.
(require "units.rkt" racket/list racket/math racket/class racket/draw pict
         "ir.rkt" "tagged.rkt" "draw-ir.rkt"
         (only-in "record-adapt.rkt" pict->display-page current-adapt-warnings)
         (only-in "runtime.rkt" placed placed? placed-x placed-y placed-rot
                  placed-pict placed-tag placed-position pin-placed))
(provide pict->page semantic-page? current-flatten-opaque?)

;; A pict is exported semantically when it says how it was built.
(define (semantic-page? p) (slide-desc? (pict-desc p)))

(define (warn! fmt . args)
  (define b (current-adapt-warnings))
  (when b (set-box! b (cons (apply format fmt args) (unbox b)))))

(define (pict->page p width height)
  (define d (pict-desc p))
  (cond
    [(slide-desc? d)
     (display-page width height
                   (ir-fill->fill (slide-desc-background d))
                   (slide-items d p width height)
                   (slide-desc-hidden? d))]
    [else (pict->display-page (lambda (dc) (draw-pict p dc 0 0)) width height)]))

;; ------------------------------------------------------------------ colors

(define (ir-color->rgba c)
  (if (rgba? c) (rgba* (rgba-r c) (rgba-g c) (rgba-b c) (rgba-a c)) (rgba* 0 0 0 1.0)))

(define (ir-fill->fill f)
  (cond
    ;; A bare color is a solid fill. The drawing path has always taken one --
    ;; `slide_canvas(~background: hex("FFFFFF"))` is what generated code
    ;; writes -- so refusing it here silently dropped every generated deck's
    ;; background on export, and a consumer that does not default to white then
    ;; painted the slide black.
    [(rgba? f) (fill:solid (ir-color->rgba f))]
    [(solid-fill? f) (fill:solid (ir-color->rgba (solid-fill-color f)))]
    [(gradient-fill? f)
     ;; Endpoints are resolved by the writer from the angle, so a unit span is
     ;; enough here; the angle is what carries the direction.
     (define a (degrees->radians (gradient-fill-angle f)))
     (fill:linear 0.0 0.0 (cos a) (sin a)
                  (for/list ([s (in-list (gradient-fill-stops f))])
                    (list (exact->inexact (car s)) (ir-color->rgba (cdr s)))))]
    ;; A picture as a fill keeps its file, so the export can write the bytes
    ;; back out rather than dropping the picture on the floor.
    [(image-fill? f) (fill:image (image-fill-src f) (image-fill-opacity f))]
    [(pattern-fill? f) (fill:solid (ir-color->rgba (pattern-fill-fg f)))]
    [else #f]))

(define (ir-line->pen l scale)
  (and (stroke? l)
       (pen* (ir-color->rgba (stroke-color l))
             (* scale (stroke-width l))
             (stroke-dash l)
             (case (stroke-cap l) [(round) 'round] [(projecting) 'square] [else 'flat])
             'miter
             (stroke-head l) (stroke-tail l) (stroke-dash-pattern l))))

;; A group with a scale changes text size along with everything else, matching
;; what the renderer draws.
(define (scale-body body factor)
  (cond
    [(or (not body) (= 1.0 factor)) body]
    [else
     (struct-copy
      text-body body
      [paras (for/list ([p (in-list (text-body-paras body))])
               (struct-copy para p
                            [runs (for/list ([r (in-list (para-runs p))])
                                    (struct-copy trun r [size (* factor (trun-size r))]))]
                            [margin-left (* factor (para-margin-left p))]
                            [indent (* factor (para-indent p))]
                            [space-before (* factor (para-space-before p))]
                            [space-after (* factor (para-space-after p))]))]
      [insets (let ([i (text-body-insets body)])
                (insets (* factor (insets-l i)) (* factor (insets-t i))
                        (* factor (insets-r i)) (* factor (insets-b i))))])]))

;; ------------------------------------------------------------------- walking

;; A transform from an element's own coordinate space to the slide's:
;; absolute = offset + scale * local.
(struct xf (ox oy sx sy) #:transparent)
(define (xf-x t v) (+ (xf-ox t) (* (xf-sx t) v)))
(define (xf-y t v) (+ (xf-oy t) (* (xf-sy t) v)))
(define (xf-factor t) (sqrt (max 0.0 (* (abs (xf-sx t)) (abs (xf-sy t))))))

(define (slide-items d whole width height)
  (append* (for/list ([pl (in-list (slide-desc-placeds d))])
             (placed-items pl (xf 0.0 0.0 1.0 1.0) width height))))

(define (placed-items pl t page-w page-h)
  (define p (placed-pict pl))
  (define d (pict-desc p))
  (define-values (local-x local-y) (placed-position pl))
  (define x (xf-x t local-x))
  (define y (xf-y t local-y))
  (define rot (placed-rot pl))
  (define f (xf-factor t))
  (define tag (placed-tag pl))
  (cond
    [(shape-desc? d) (shape-items d x y rot t f tag)]
    [(text-desc? d)
     (list (it:textbox x y (* (xf-sx t) (text-desc-width d)) (* (xf-sy t) (text-desc-height d))
                       rot (scale-body (text-desc-body d) f) tag))]
    [(image-desc? d)
     (list (it:picture x y (* (xf-sx t) (image-desc-width d))
                       (* (xf-sy t) (image-desc-height d)) rot
                       (image-desc-src d) (image-desc-crop d)
                       (image-desc-flip-h? d) (image-desc-flip-v? d)
                       (ir-line->pen (image-desc-line d) f)
                       (image-desc-opacity d) tag))]
    [(group-desc? d) (list (group-item d x y rot t tag page-w page-h))]
    ;; A table is drawn rather than described, but it flattens into shapes and
    ;; text that read correctly, so it keeps that treatment.
    [(table-desc? d) (raw-items pl page-w page-h)]
    [else (opaque-items pl t x y rot tag page-w page-h)]))

;; How much of an element's structure the editor is allowed to see.
;;
;; A pict with no descriptor -- `(vc-append 5 a b c)` inside an `at`, say -- has
;; no structure we can sync. Flattening its *drawing* gives a pile of separate
;; shapes, every one of which can be dragged in Keynote and none of which can be
;; moved back, because the only thing the source names is the enclosing `at`.
;; Emitting one picture instead makes the file's affordances match what the tool
;; can honor: one object per `at`, and dragging it lands on numbers that exist.
;;
;; The cost is that its text becomes pixels. That is why this is off for a
;; one-way export, where nothing will be synced and separate shapes are strictly
;; better.
(define current-flatten-opaque? (make-parameter #t))

;; Rendered at twice its final size, so it still looks sharp on a projector.
(define FLATTEN-OVERSAMPLE 2.0)

(define (opaque-items pl t x y rot tag page-w page-h)
  (cond
    [(not (current-flatten-opaque?)) (raw-items pl page-w page-h)]
    [else
     (define p (placed-pict pl))
     (define w (* (abs (xf-sx t)) (pict-width p)))
     (define h (* (abs (xf-sy t)) (pict-height p)))
     (cond
       [(or (< w 0.01) (< h 0.01)) '()]
       [else
        (define-values (argb iw ih)
          (pict->argb p (* FLATTEN-OVERSAMPLE (max 1.0 (xf-factor t)))))
        (warn! "~a has no structure to sync, so it is exported as one picture"
               (or tag "an unnamed element"))
        (list (it:image x y w h rot argb iw ih tag))])]))

;; Draws `p` on its own and returns its pixels.
(define (pict->argb p oversample)
  (define iw (max 1 (exact-ceiling (* oversample (pict-width p)))))
  (define ih (max 1 (exact-ceiling (* oversample (pict-height p)))))
  (define bm (make-bitmap iw ih))
  (define dc (new bitmap-dc% [bitmap bm]))
  (send dc set-smoothing (quote smoothed))
  (draw-pict (scale p (/ iw (pict-width p)) (/ ih (pict-height p))) dc 0 0)
  (define bs (make-bytes (* 4 iw ih)))
  (send bm get-argb-pixels 0 0 iw ih bs)
  (values bs iw ih))

(define (shape-items d x y rot t f tag)
  (define w (* (xf-sx t) (shape-desc-width d)))
  (define h (* (xf-sy t) (shape-desc-height d)))
  (define geom (shape-desc-geom d))
  (define fill (ir-fill->fill (shape-desc-fill d)))
  (define pen (ir-line->pen (shape-desc-line d) f))
  (define body (scale-body (shape-desc-body d) f))
  (cond
    [(preset-geom? geom)
     (list (it:preset x y w h rot (preset-geom-name geom) (preset-geom-adjust geom)
                      (shape-desc-flip-h? d) (shape-desc-flip-v? d)
                      fill pen body tag))]
    ;; Custom geometry has no preset to name, so it stays a drawn path; the text
    ;; still travels with it.
    [else
     (define segs (custom-geom->segs geom x y w h))
     (list (it:shape-path segs fill pen (list x y w h) rot
                          (shape-desc-flip-h? d) (shape-desc-flip-v? d)
                          body tag))]))

;; Custom geometry arrives in its own coordinate space; scale it onto the box.
(define (custom-geom->segs g x y w h)
  (define gw (custom-geom-w g))
  (define gh (custom-geom-h g))
  ;; A path space of 0 -- which is what an omitted `w` or `h` means -- says the
  ;; coordinates are EMU within the shape, not a space to stretch onto the box.
  ;; Clamping the divisor to 1 instead multiplied them by the box's size, which
  ;; is a 21600-fold blow-up on the decks that write paths this way.
  (define (X v) (if (zero? gw) (+ x (emu->pt v)) (+ x (* w (/ (exact->inexact v) gw)))))
  (define (Y v) (if (zero? gh) (+ y (emu->pt v)) (+ y (* h (/ (exact->inexact v) gh)))))
  (append*
   (for/list ([path (in-list (custom-geom-paths g))])
     (for/list ([cmd (in-list path)])
       (case (car cmd)
         [(move) (seg:move (X (car (second cmd))) (Y (cdr (second cmd))))]
         [(line) (seg:line (X (car (second cmd))) (Y (cdr (second cmd))))]
         [(curve) (let ([ps (rest cmd)])
                    (if (= 3 (length ps))
                        (seg:curve (X (car (first ps))) (Y (cdr (first ps)))
                                   (X (car (second ps))) (Y (cdr (second ps)))
                                   (X (car (third ps))) (Y (cdr (third ps))))
                        (seg:close)))]
         [(quad) (let ([ps (rest cmd)])
                   (if (= 2 (length ps))
                       (seg:curve (X (car (first ps))) (Y (cdr (first ps)))
                                  (X (car (first ps))) (Y (cdr (first ps)))
                                  (X (car (second ps))) (Y (cdr (second ps))))
                       (seg:close)))]
         [else (seg:close)])))))

;; A group stays a group: its children are laid out in the slide's coordinates,
;; as they already were, and wrapped in one item.
(define (group-item d gx gy rot t tag page-w page-h)
  (define w (* (abs (xf-sx t)) (group-desc-width d)))
  (define h (* (abs (xf-sy t)) (group-desc-height d)))
  (it:group gx gy w h rot
            (group-desc-flip-h? d) (group-desc-flip-v? d)
            (group-items d gx gy t page-w page-h) tag))

(define (group-items d gx gy t page-w page-h)
  (define cw (max 1e-9 (group-desc-child-width d)))
  (define ch (max 1e-9 (group-desc-child-height d)))
  (define inner-sx (/ (group-desc-width d) cw))
  (define inner-sy (/ (group-desc-height d) ch))
  (define sx (* (xf-sx t) inner-sx))
  (define sy (* (xf-sy t) inner-sy))
  (define inner
    (xf (- gx (* sx (group-desc-child-x d)))
        (- gy (* sy (group-desc-child-y d)))
        sx sy))
  (append* (for/list ([pl (in-list (group-desc-placeds d))])
             (placed-items pl inner page-w page-h))))

;; Renders one element by itself, through the runtime's own placement, so the
;; ink lands exactly where the slide put it.
(define (raw-items pl page-w page-h)
  (define composed (pin-placed (blank page-w page-h) pl))
  (display-page-items
   (pict->display-page (lambda (dc) (draw-pict composed dc 0 0)) page-w page-h)))
