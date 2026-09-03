#lang racket/base
;; The state both sides of a sync are compared in.
;;
;; A sync needs one vocabulary that a program and a .pptx can both be reduced
;; to: what elements exist, where they are, and enough of what they look like to
;; recognize one after an editor has renamed it. That is this.
(require racket/file racket/path racket/list racket/string racket/format racket/math
         "ir.rkt" "draw-ir.rkt")
(provide (struct-out el-state) (struct-out slide-state)
         items->slide-state deck->slide-states
         el-geometry el-geometry-same? el-signature signature-distance
         write-sync-base read-sync-base)

;; `kind` is 'shape, 'text, 'picture or 'other. `text` is the element's visible
;; text, flattened, which is the strongest signal for recognizing it again.
;; `paint` is a short digest of its fill, and `z` its position in paint order.
(struct el-state (tag kind x y w h rot text paint z) #:prefab)
(struct slide-state (index width height elements) #:prefab)

(define GEOM-EPSILON 0.05)

(define (el-geometry e)
  (list (el-state-x e) (el-state-y e) (el-state-w e) (el-state-h e) (el-state-rot e)))

;; PowerPoint rounds to EMU, so a sync must not treat that as an edit.
(define (el-geometry-same? a b)
  (for/and ([p (in-list (el-geometry a))] [q (in-list (el-geometry b))])
    (< (abs (- p q)) GEOM-EPSILON)))

;; ------------------------------------------------------- from a display list

(define (items->slide-state index width height items)
  (slide-state index width height
               (for/list ([i (in-list items)] [z (in-naturals)]
                          #:when (semantic-item? i))
                 (item->el-state i z))))

(define (item->el-state i z)
  (cond
    [(it:preset? i)
     (el-state (it:preset-tag i) 'shape (it:preset-x i) (it:preset-y i)
               (it:preset-w i) (it:preset-h i) (it:preset-rot i)
               (body-text (it:preset-body i)) (fill-digest (it:preset-fill i)) z)]
    [(it:textbox? i)
     (el-state (it:textbox-tag i) 'text (it:textbox-x i) (it:textbox-y i)
               (it:textbox-w i) (it:textbox-h i) (it:textbox-rot i)
               (body-text (it:textbox-body i)) "" z)]
    [(it:picture? i)
     (el-state (it:picture-tag i) 'picture (it:picture-x i) (it:picture-y i)
               (it:picture-w i) (it:picture-h i) (it:picture-rot i)
               "" (format "~a" (it:picture-src i)) z)]
    ;; A flattened element is a picture on both sides of the sync, so it is
    ;; described as one here too and the signature matcher agrees.
    [(it:image? i)
     (el-state (it:image-tag i) 'picture (it:image-x i) (it:image-y i)
               (it:image-w i) (it:image-h i) (it:image-rot i) "" "flattened" z)]
    [else
     (define-values (x y w h) (apply values (it:shape-path-box i)))
     (el-state (it:shape-path-tag i) 'shape x y w h 0.0
               (body-text (it:shape-path-body i))
               (fill-digest (it:shape-path-fill i)) z)]))

(define (body-text body)
  (if (not body)
      ""
      (string-join
       (for/list ([p (in-list (text-body-paras body))])
         (apply string-append (for/list ([r (in-list (para-runs p))]) (trun-text r))))
       "\n")))

(define (fill-digest f)
  (cond
    [(fill:solid? f) (let ([c (fill:solid-color f)])
                       (format "~a,~a,~a" (round* (rgba*-r c)) (round* (rgba*-g c))
                               (round* (rgba*-b c))))]
    [(fill:linear? f) "gradient"]
    [(fill:radial? f) "gradient"]
    [else ""]))

(define (round* v) (inexact->exact (round v)))

;; ------------------------------------------------------------ from a deck IR

;; The importer's view of a .pptx, reduced to the same vocabulary. Inherited
;; layout and master shapes are left out: they are not the slide's to sync.
;; `include-inherited?` folds in the shapes the layout and master paint behind
;; the slide. A sync leaves those alone, but a structural comparison needs them:
;; an export writes them as ordinary slide shapes, so they come back on the
;; other side as the slide's own.
(define (deck->slide-states d #:include-inherited? [include-inherited? #f])
  (for/list ([s (in-list (deck-slides d))])
    (define acc '())
    (define z 0)
    (let walk ([es (if include-inherited? (slide-all-elements s) (slide-elements s))])
      (for ([e (in-list es)])
        (cond
          [(group? e) (walk (group-children e))]
          [else
           (define b (element-bbox e))
           (define tag (let ([n (element-name e)]) (and (not (string=? "" n)) n)))
           (set! acc
                 (cons (el-state tag
                                 (cond [(picture? e) 'picture]
                                       [(shape? e) (if (and (shape-body e)
                                                            (not (shape-fill e))
                                                            (not (shape-line e)))
                                                       'text 'shape)]
                                       [else 'other])
                                 (bbox-x b) (bbox-y b) (bbox-w b) (bbox-h b) (bbox-rot b)
                                 (if (shape? e) (body-text (shape-body e)) "")
                                 (if (shape? e) (ir-fill-digest (shape-fill e)) "")
                                 z)
                       acc))
           (set! z (add1 z))])))
    (slide-state (slide-index s) (slide-width s) (slide-height s) (reverse acc))))

(define (ir-fill-digest f)
  (cond
    [(solid-fill? f) (let ([c (solid-fill-color f)])
                       (format "~a,~a,~a" (round* (rgba-r c)) (round* (rgba-g c))
                               (round* (rgba-b c))))]
    [(gradient-fill? f) "gradient"]
    [else ""]))

;; ------------------------------------------------------------ recognition

;; What an element looks like, for matching it after an editor renamed it.
(define (el-signature e)
  (list (el-state-kind e) (el-state-text e) (el-state-paint e)))

;; How unlike two elements are. Text and kind dominate; position counts least,
;; because a shape having moved is the thing being detected.
(define (signature-distance a b #:slide-size [size 1000.0])
  (cond
    [(not (eq? (el-state-kind a) (el-state-kind b))) +inf.0]
    [else
     (define (norm v) (/ (abs v) size))
     (+ (if (string=? (el-state-text a) (el-state-text b)) 0.0 6.0)
        (if (string=? (el-state-paint a) (el-state-paint b)) 0.0 2.0)
        (* 3.0 (+ (norm (- (el-state-w a) (el-state-w b)))
                  (norm (- (el-state-h a) (el-state-h b)))))
        (* 0.5 (min 1.0 (/ (abs (- (el-state-z a) (el-state-z b))) 8.0)))
        (* 0.4 (+ (norm (- (el-state-x a) (el-state-x b)))
                  (norm (- (el-state-y a) (el-state-y b))))))]))

;; --------------------------------------------------------------- base file

;; The base is what both sides agreed on at the last successful sync. It is
;; readable and belongs in version control next to the program, so a conflict is
;; a diff a person can read.
(define (write-sync-base path states #:program program #:deck deck)
  ;; The base sits in a scratch directory, which need not exist yet.
  (let ([dir (path-only (path->complete-path path))])
    (when dir (make-directory* dir)))
  (call-with-output-file path #:exists 'replace
    (lambda (o)
      (fprintf o ";; glide-pptx sync base -- what the program and the deck agreed\n")
      (fprintf o ";; on at the last sync. Edit at your own risk; delete to resync\n")
      (fprintf o ";; from scratch.\n")
      (writeln* o `(sync-base (version 1)
                              (program ,program)
                              (deck ,deck)
                              (slides ,@states))))))

(define (writeln* o v) (write v o) (newline o))

;; Returns (values states program deck), or (values #f #f #f) when there is none.
(define (read-sync-base path)
  (cond
    [(not (file-exists? path)) (values #f #f #f)]
    [else
     (define v (with-handlers ([exn:fail? (lambda (_e) #f)])
                 (call-with-input-file path read)))
     (cond
       [(and (list? v) (pair? v) (eq? 'sync-base (car v)))
        (define (field name)
          (define hit (assq name (cdr v)))
          (and hit (cdr hit)))
        (values (field 'slides)
                (let ([p (field 'program)]) (and (pair? p) (car p)))
                (let ([p (field 'deck)]) (and (pair? p) (car p))))]
       [else (values #f #f #f)])]))
