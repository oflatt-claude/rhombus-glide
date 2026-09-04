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
;; `flip-h?`/`flip-v?` are here because dragging a line's endpoint past the
;; other end mirrors the shape rather than moving it: without them that edit was
;; invisible to a merge.
(struct el-state (tag kind x y w h rot flip-h? flip-v? text paint style z) #:prefab)
(struct slide-state (index width height elements) #:prefab)

(define GEOM-EPSILON 0.05)

(define (el-geometry e)
  (list (el-state-x e) (el-state-y e) (el-state-w e) (el-state-h e) (el-state-rot e)
        (el-state-flip-h? e) (el-state-flip-v? e)))

;; PowerPoint rounds to EMU, so a sync must not treat that as an edit.
;; A rotation is an angle, so 360 and 0 are the same rotation and so are -45 and
;; 315. Comparing them as plain numbers reported differences that were not.
(define (turn-same? a b)
  (define d (- (modulo* a 360.0) (modulo* b 360.0)))
  (or (< (abs d) GEOM-EPSILON) (< (abs (- 360.0 (abs d))) GEOM-EPSILON)))

(define (modulo* v m)
  (define r (- v (* m (floor (/ v m)))))
  (if (< r 0) (+ r m) r))

(define (el-geometry-same? a b)
  (and (for/and ([p (in-list (list (el-state-x a) (el-state-y a)
                                   (el-state-w a) (el-state-h a)))]
                 [q (in-list (list (el-state-x b) (el-state-y b)
                                   (el-state-w b) (el-state-h b)))])
         (< (abs (- p q)) GEOM-EPSILON))
       (turn-same? (el-state-rot a) (el-state-rot b))
       (eq? (and (el-state-flip-h? a) #t) (and (el-state-flip-h? b) #t))
       (eq? (and (el-state-flip-v? a) #t) (and (el-state-flip-v? b) #t))))

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
               (it:preset-flip-h? i) (it:preset-flip-v? i)
               (body-text (it:preset-body i)) (fill-digest (it:preset-fill i))
               (append (fill-style (it:preset-fill i)) (pen-style (it:preset-pen i))
                       (body-style (it:preset-body i)))
               z)]
    [(it:textbox? i)
     (el-state (it:textbox-tag i) 'text (it:textbox-x i) (it:textbox-y i)
               (it:textbox-w i) (it:textbox-h i) (it:textbox-rot i) #f #f
               (body-text (it:textbox-body i)) ""
               (body-style (it:textbox-body i)) z)]
    [(it:picture? i)
     (el-state (it:picture-tag i) 'picture (it:picture-x i) (it:picture-y i)
               (it:picture-w i) (it:picture-h i) (it:picture-rot i)
               (it:picture-flip-h? i) (it:picture-flip-v? i)
               "" (format "~a" (it:picture-src i))
               (append (pen-style (it:picture-pen i))
                       (list (cons 'opacity (/ (round (* 100.0 (it:picture-opacity i))) 100.0))))
               z)]
    ;; A flattened element is a picture on both sides of the sync, so it is
    ;; described as one here too and the signature matcher agrees.
    [(it:image? i)
     (el-state (it:image-tag i) 'picture (it:image-x i) (it:image-y i)
               (it:image-w i) (it:image-h i) (it:image-rot i) #f #f
               "" "flattened" '() z)]
    ;; A group is one element to drag, whatever it holds.
    [(it:group? i)
     (el-state (it:group-tag i) 'group (it:group-x i) (it:group-y i)
               (it:group-w i) (it:group-h i) (it:group-rot i)
               (it:group-flip-h? i) (it:group-flip-v? i) "" "group" '() z)]
    [(it:shape-path? i)
     (define-values (x y w h) (apply values (it:shape-path-box i)))
     (el-state (it:shape-path-tag i) 'shape x y w h (it:shape-path-rot i)
               (it:shape-path-flip-h? i) (it:shape-path-flip-v? i)
               (body-text (it:shape-path-body i))
               (fill-digest (it:shape-path-fill i))
               (append (fill-style (it:shape-path-fill i))
                       (pen-style (it:shape-path-pen i))
                       (body-style (it:shape-path-body i)))
               z)]
    ;; Not a fall-through: a new kind of semantic item should say so here rather
    ;; than be read as whatever the last branch happened to be. This branch used
    ;; to be the shape-path one, and a group -- which became a semantic item when
    ;; groups started exporting as groups -- was read as a shape-path and
    ;; crashed.
    [else (error 'sync "no state for ~a" i)]))

;; Normalized, because the same text can be spelled two ways: macOS hands back
;; decomposed forms where the file had composed ones, and a text that only
;; differs that way is not a text the user edited. Comparing the raw strings
;; reported an edit on every sync, on every shape whose text has an accent or a
;; combining mark, and it could never be applied or settled.
(define (body-text body)
  (if (not body)
      ""
      (string-normalize-nfc
       (string-join
        (for/list ([p (in-list (text-body-paras body))])
          (apply string-append (for/list ([r (in-list (para-runs p))]) (trun-text r))))
        "\n"))))

(define (fill-digest f)
  (cond
    [(fill:solid? f) (let ([c (fill:solid-color f)])
                       (format "~a,~a,~a" (round* (rgba*-r c)) (round* (rgba*-g c))
                               (round* (rgba*-b c))))]
    [(fill:linear? f) "gradient"]
    [(fill:radial? f) "gradient"]
    [else ""]))

(define (round* v) (inexact->exact (round v)))

;; ------------------------------------------------------------------- style

;; What an element looks like, as named properties, so a merge can say *which*
;; one changed rather than only that something did. Both sides build this the
;; same way -- one from the display list a program draws, one from a deck's IR --
;; which is what makes them comparable.
;;
;; Appearance is the code's to own, so these are reported and only written where
;; the source holds a literal to write to. Reporting them at all is the point:
;; recolouring a shape in the editor used to vanish without a word.
;; A colour is `rgba*` when it came from a display list and `rgba` when it came
;; from a deck's IR. Both sides build the same properties, so this takes either.
(define (hex-of c)
  (cond
    [(rgba*? c) (format "~a~a~a" (byte->hex (rgba*-r c)) (byte->hex (rgba*-g c))
                        (byte->hex (rgba*-b c)))]
    [(rgba? c) (format "~a~a~a" (byte->hex (rgba-r c)) (byte->hex (rgba-g c))
                       (byte->hex (rgba-b c)))]
    [else #f]))

(define (byte->hex v)
  (define n (max 0 (min 255 (inexact->exact (round v)))))
  (string-upcase (if (< n 16) (format "0~x" n) (format "~x" n))))

(define (pen-style p)
  (if (not p)
      '()
      (append (let ([h (hex-of (pen*-color p))]) (if h (list (cons 'line h)) '()))
              (list (cons 'line-width (/ (round (* 100.0 (pen*-width p))) 100.0))
                    (cons 'dash (format "~a" (pen*-dash p)))))))

;; A colour's alpha is part of how it looks, so a shape made translucent in the
;; editor is a change like any other.
(define (alpha-of c)
  (define a (cond [(rgba*? c) (rgba*-a c)] [(rgba? c) (rgba-a c)] [else 1.0]))
  (/ (round (* 100.0 a)) 100.0))

(define (fill-style f)
  (cond
    [(fill:solid? f) (let ([h (hex-of (fill:solid-color f))])
                       (if h (list (cons 'fill h)
                                   (cons 'fill-opacity (alpha-of (fill:solid-color f))))
                           '()))]
    [(or (fill:linear? f) (fill:radial? f)) (list (cons 'fill "gradient"))]
    [else '()]))

;; The first run's typeface and size stand for the body: an edit to one run of
;; many is refused anyway, and this is what a report needs to name.
(define (body-style body)
  (define r (and body
                 (for/or ([p (in-list (text-body-paras body))])
                   (and (pair? (para-runs p)) (first (para-runs p))))))
  (if (not r)
      '()
      (list (cons 'font (trun-family r))
            (cons 'size (/ (round (* 10.0 (trun-size r))) 10.0))
            (cons 'bold (and (trun-bold? r) #t))
            (cons 'italic (and (trun-italic? r) #t))
            (cons 'text-color (or (hex-of (trun-color r)) "")))))

;; ------------------------------------------------------------ from a deck IR

;; The importer's view of a .pptx, reduced to the same vocabulary. Inherited
;; layout and master shapes are left out: they are not the slide's to sync.
;; `include-inherited?` folds in the shapes the layout and master paint behind
;; the slide. A sync leaves those alone, but a structural comparison needs them:
;; an export writes them as ordinary slide shapes, so they come back on the
;; other side as the slide's own.
;; `descend-groups?` says whether a group is its children or one element. For a
;; sync it is one element -- that is what a program's `group_pict` draws and what
;; the editor drags -- and the two sides have to agree, or a slide's tags do not
;; overlap and the merge reads it as a different slide. A structural comparison
;; wants the children, since that is where a lost field would hide.
(define (deck->slide-states d #:include-inherited? [include-inherited? #f]
                            #:descend-groups? [descend-groups? #f])
  (for/list ([s (in-list (deck-slides d))])
    (define acc '())
    (define z 0)
    (let walk ([es (if include-inherited? (slide-all-elements s) (slide-elements s))])
      (for ([e (in-list es)])
        (cond
          [(and (group? e) descend-groups?) (walk (group-children e))]
          [else
           (define b (element-bbox e))
           (define tag (let ([n (element-name e)]) (and (not (string=? "" n)) n)))
           (set! acc
                 (cons (el-state tag
                                 (cond [(group? e) 'group]
                                       [(tbl? e) 'table]
                                       [(picture? e) 'picture]
                                       [(shape? e) (if (and (shape-body e)
                                                            (not (shape-fill e))
                                                            (not (shape-line e)))
                                                       'text 'shape)]
                                       [else 'other])
                                 (bbox-x b) (bbox-y b) (bbox-w b) (bbox-h b) (bbox-rot b)
                                 (bbox-flip-h? b) (bbox-flip-v? b)
                                 (if (shape? e) (body-text (shape-body e)) "")
                                 (if (shape? e) (ir-fill-digest (shape-fill e)) "")
                                 (ir-style e)
                                 z)
                       acc))
           (set! z (add1 z))])))
    (slide-state (slide-index s) (slide-width s) (slide-height s) (reverse acc))))

;; The same properties as `fill-style`/`pen-style`/`body-style`, from a deck's
;; IR rather than from a display list, so the two sides compare.
(define (ir-style e)
  (define hex hex-of)
  (define fill
    (let ([f (and (shape? e) (shape-fill e))])
      (cond
        [(solid-fill? f) (let ([h (hex (solid-fill-color f))])
                           (if h (list (cons 'fill h)
                                       (cons 'fill-opacity (alpha-of (solid-fill-color f))))
                               '()))]
        [(gradient-fill? f) (list (cons 'fill "gradient"))]
        [else '()])))
  (define line
    (let ([l (and (or (shape? e) (picture? e))
                  (if (shape? e) (shape-line e) (picture-line e)))])
      (if (stroke? l)
          (append (let ([h (hex (stroke-color l))]) (if h (list (cons 'line h)) '()))
                  (list (cons 'line-width
                              (let ([w (stroke-width l)])
                                (if (real? w) (/ (round (* 100.0 w)) 100.0) 0.0)))
                        (cons 'dash (format "~a" (let ([d (stroke-dash l)])
                                                   (if (eq? 'inherit d) 'solid d))))))
          '())))
  (define text (if (shape? e) (body-style (shape-body e)) '()))
  (define opacity
    (if (picture? e)
        (list (cons 'opacity (/ (round (* 100.0 (picture-opacity e))) 100.0)))
        '()))
  (append fill line text opacity))

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
