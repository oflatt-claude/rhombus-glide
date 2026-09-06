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
         nth-property
         write-sync-base read-sync-base write-atomically)

;; `kind` is 'shape, 'text, 'picture or 'other. `text` is the element's visible
;; text, flattened, which is the strongest signal for recognizing it again.
;; `paint` is a short digest of its fill, and `z` its position in paint order.
;; `flip-h?`/`flip-v?` are here because dragging a line's endpoint past the
;; other end mirrors the shape rather than moving it: without them that edit was
;; invisible to a merge.
(struct el-state (tag kind x y w h rot flip-h? flip-v? text paint style z) #:prefab)
;; `background` is the slide's own paint, as a colour, "gradient", or #f for
;; none: the canvas states it, and an editor can change it without touching a
;; single element.
(struct slide-state (index width height elements background hidden?) #:prefab)

;; How far a number may move without anyone having moved anything.
;;
;; PowerPoint rounds to EMU, which is nothing. LibreOffice keeps its geometry in
;; hundredths of a millimetre -- 0.0283pt -- so every coordinate shifts by up to
;; that much each way when it saves a deck it did not write: measured on one
;; deck, 242.3 became 242.2488 and 312.373 became 312.321. At 0.05 those slipped
;; past and a deck nobody had touched came back with 119 edits in it.
;;
;; A tenth of a point is a five-hundredth of an inch. Nothing anyone does in an
;; editor is that small: the arrow keys move a shape by a millimetre, which is
;; twenty-eight times this.
(define GEOM-EPSILON 0.15)

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

(define (items->slide-state index width height items #:background [bg #f]
                            #:hidden? [hidden? #f])
  (slide-state index width height
               (for/list ([i (in-list items)] [z (in-naturals)]
                          #:when (semantic-item? i))
                 (item->el-state i z))
               (paint-name (fill-style bg))
               (and hidden? #t)))

;; A paint as one value: the colour it is, "gradient" for one, or #f for none.
;; Opacity rides along, since a background is written as one argument.
(define (paint-name style)
  (define c (assq 'fill style))
  (define o (assq 'fill-opacity style))
  (and c (if (and o (< (cdr o) 0.999)) (list (cdr c) (cdr o)) (cdr c))))

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
                       (list (cons 'opacity (round-to (it:picture-opacity i) 100.0))
                             (cons 'crop (crop-style (it:picture-crop i)))
                             (cons 'image (bytes-of (it:picture-src i)))))
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
                    (cons 'dash (format "~a" (pen*-dash p)))
                    (cons 'cap (pen-cap-name (pen*-cap p)))
                    (cons 'head (end-style (pen*-head p)))
                    (cons 'tail (end-style (pen*-tail p)))))))

;; The two sides spell a line's cap differently -- the drawing side calls
;; PowerPoint's `projecting` a square end -- so they are compared under the
;; name the program writes.
(define (pen-cap-name c)
  (case c [(round) 'round] [(square projecting) 'projecting] [else 'flat]))

;; What is on the end of a line: an arrowhead the editor put there is an edit
;; like any other. `#f` is one of the values, so it is reported as such rather
;; than left out -- both sides of the comparison read it the same way.
(define (end-style e)
  (and (line-end? e)
       (list (line-end-kind e) (line-end-width e) (line-end-length e))))

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
;; A property of one run or paragraph beyond the first is named by its number:
;; `(font 2)` is the second run's typeface. The first of each keeps its plain
;; name, so a report about a body of one run reads the way it always did.
(define (nth-property name i) (if (= i 1) name (list name i)))

;; Only what the shape says for itself. A property it inherits -- from its
;; placeholder, the layout, the master, the theme -- is not a statement about
;; it, and comparing inherited values reads an editor's terse export as a
;; hundred edits nobody made, or worse writes the inherited value back into the
;; program. `'all` is what a program's own text carries.
(define (only-stated style stated)
  (if (eq? 'all stated)
      style
      (for/list ([kv (in-list style)]
                 #:when (memq (property-of (car kv)) stated))
        kv)))

;; A property is named either by a symbol or, after the first run or paragraph,
;; by a list of the name and the number.
(define (property-of k) (if (pair? k) (car k) k))

;; Everything the font panel can do to a run. Underline, strike, letter
;; spacing, capitals and a raised baseline were each in the representation and
;; each dropped by the merge.
(define (run-style r i)
  (only-stated
   (list (cons (nth-property 'font i) (trun-family r))
         ;; Hundredths, which is what `sz` keeps: rounding to tenths made a
         ;; value land on either side of a boundary depending on which way it
         ;; had been through the writer.
         (cons (nth-property 'size i) (round-to (trun-size r) 100.0))
         (cons (nth-property 'bold i) (and (trun-bold? r) #t))
         (cons (nth-property 'italic i) (and (trun-italic? r) #t))
         (cons (nth-property 'underline i) (and (trun-underline? r) #t))
         (cons (nth-property 'strike i) (and (trun-strike? r) #t))
         (cons (nth-property 'spacing i) (round-to (trun-spacing r) 100.0))
         (cons (nth-property 'caps i) (trun-caps r))
         (cons (nth-property 'baseline i) (round-to (trun-baseline r) 1000.0))
         (cons (nth-property 'text-color i) (or (hex-of (trun-color r)) "")))
   (trun-stated r)))

;; And everything the paragraph panel can do: its level in a list, the indents
;; that hang its bullet, and the bullet itself.
(define (para-style p i)
  (only-stated
   (list (cons (nth-property 'align i) (para-align p))
         (cons (nth-property 'line-spacing i)
               (let ([ls (para-line-spacing p)])
                 (cons (car ls) (round-to (cdr ls) 1000.0))))
         (cons (nth-property 'space-before i) (round-to (para-space-before p) 100.0))
         (cons (nth-property 'space-after i) (round-to (para-space-after p) 100.0))
         (cons (nth-property 'level i) (inexact->exact (round (para-level p))))
         (cons (nth-property 'margin-left i) (round-to (para-margin-left p) 10.0))
         (cons (nth-property 'indent i) (round-to (para-indent p) 10.0))
         (cons (nth-property 'bullet i) (bullet-style (para-bullet p))))
   (para-stated p)))

;; A bullet as the five things that describe it, so a list turned from dots to
;; numbers is a difference the merge can see.
(define (bullet-style b)
  (and (bullet? b)
       (not (eq? 'none (bullet-kind b)))
       (list (bullet-kind b) (bullet-char b) (bullet-font b)
             (and (bullet-size-frac b) (round-to (bullet-size-frac b) 1000.0))
             (and (bullet-color b) (hex-of (bullet-color b))))))

(define (body-style body)
  (define paras (and body (text-body-paras body)))
  (define p (and (pair? paras) (first paras)))
  (define r (and paras
                 (for/or ([p (in-list paras)])
                   (and (pair? (para-runs p)) (first (para-runs p))))))
  (append
   ;; How the text sits in its box, which the editor's inspector can change
   ;; without touching a word of it.
   (if body
       (only-stated
        (list (cons 'anchor (text-body-anchor body))
              (cons 'wrap (and (text-body-wrap? body) #t))
              (cons 'autofit (text-body-autofit body))
              (cons 'insets (let ([i (text-body-insets body)])
                              (map (lambda (x) (round-to x 100.0))
                                   (list (insets-l i) (insets-t i)
                                         (insets-r i) (insets-b i))))))
        (text-body-stated body))
       '())
   ;; Every paragraph, and every run in them. Centring text is one of the first
   ;; things anyone does in an editor, and bolding one word of a line is
   ;; another -- and each was dropped without a word, the first because
   ;; paragraphs were not compared at all and the second because only the first
   ;; run was.
   (append* (for/list ([p (in-list (or paras '()))] [i (in-naturals 1)])
              (para-style p i)))
   (append* (for/list ([r (in-list (all-runs paras))] [i (in-naturals 1)])
              (run-style r i)))
   ;; The first run's own properties stand for the body when it has none of its
   ;; own to report -- an empty paragraph carries a typeface too.
   (if (and r (null? (all-runs paras))) (run-style r 1) '())
   (if p '() '())))

;; Every run of a body, paragraph by paragraph, which is the order the source
;; writes them in.
(define (all-runs paras)
  (append* (for/list ([p (in-list (or paras '()))]) (para-runs p))))

;; Rounded, so that a value that differs in the last decimal place is not read
;; as an edit.
;; Which picture it is, as the size of the file. The two sides name the same
;; bytes differently -- the program a path, a deck a part inside itself -- so
;; the file is what they have in common. Size rather than a digest because it
;; is a stat rather than a read, and a picture swapped for another of exactly
;; the same size is a miss worth taking.
(define (bytes-of p)
  (and p (file-exists? p) (file-size p)))

;; How much of a picture is cropped away, as four fractions or #f for none. A
;; crop is a value the editor sets with the crop tool and the program states as
;; a list, so both ends of it are comparable.
(define (crop-style c)
  (and (list? c) (= 4 (length c)) (for/list ([v (in-list c)]) (round-to v 10000.0))))

(define (round-to v scale) (/ (round (* scale (exact->inexact v))) scale))

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
                                 (ir-style e (deck-media-dir d))
                                 z)
                       acc))
           (set! z (add1 z))])))
    (slide-state (slide-index s) (slide-width s) (slide-height s) (reverse acc)
                 (paint-name (ir-fill-style (slide-background s)))
                 (and (slide-hidden? s) #t))))

;; The same as `fill-style`, for a deck's own fills.
(define (ir-fill-style f)
  (cond
    [(solid-fill? f) (let ([h (hex-of (solid-fill-color f))])
                       (if h (list (cons 'fill h)
                                   (cons 'fill-opacity (alpha-of (solid-fill-color f))))
                           '()))]
    [(gradient-fill? f) (list (cons 'fill "gradient"))]
    [else '()]))

;; The same properties as `fill-style`/`pen-style`/`body-style`, from a deck's
;; IR rather than from a display list, so the two sides compare.
(define (ir-style e [media-dir #f])
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
                                                   (if (eq? 'inherit d) 'solid d))))
                        (cons 'cap (pen-cap-name (stroke-cap l)))
                        (cons 'head (end-style (stroke-head l)))
                        (cons 'tail (end-style (stroke-tail l)))))
          '())))
  (define text (if (shape? e) (body-style (shape-body e)) '()))
  (define opacity
    (if (picture? e)
        (list (cons 'opacity (round-to (picture-opacity e) 100.0))
              (cons 'crop (crop-style (picture-crop e)))
              (cons 'image (bytes-of (and media-dir (picture-src e)
                                          (build-path media-dir (picture-src e))))))
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
;; Bumped whenever a state carries something it did not before. A base written
;; by an older version is not read: resyncing from scratch is what it says to do
;; when the base is unusable, and it is cheap.
(define BASE-VERSION 3)

;; Written beside the file and renamed over it, rather than into it. A write
;; that stops halfway -- a Ctrl-C, a full disk, the machine going down -- leaves
;; half a file where the whole one was, and for the program there is nothing to
;; recover it from: it is the source, and there is one copy. A rename either
;; happened or it did not.
;;
;; The mode goes across with it. The temporary file is made with whatever the
;; umask says, so a program someone had kept to themselves would come back
;; readable by everyone.
(define (write-atomically path write!)
  (define mode (and (file-exists? path)
                    (with-handlers ([exn:fail? (lambda (_e) #f)])
                      (file-or-directory-permissions path 'bits))))
  (call-with-atomic-output-file path (lambda (o _tmp) (write! o)))
  (when mode
    (with-handlers ([exn:fail? (lambda (_e) (void))])
      (file-or-directory-permissions path mode))))

(define (write-sync-base path states #:program program #:deck deck)
  ;; The base sits in a scratch directory, which need not exist yet.
  (let ([dir (path-only (path->complete-path path))])
    (when dir (make-directory* dir)))
  ;; Renamed over the old one rather than written into it: a base half written
  ;; is a base that cannot be read, and reading it is what tells an edit from a
  ;; deck that states what it always stated.
  (write-atomically path
    (lambda (o)
      (fprintf o ";; glide-pptx sync base -- what the program and the deck agreed\n")
      (fprintf o ";; on at the last sync. Edit at your own risk; delete to resync\n")
      (fprintf o ";; from scratch.\n")
      (writeln* o `(sync-base (version ,BASE-VERSION)
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
        (cond
          [(not (equal? (field 'version) (list BASE-VERSION))) (values #f #f #f)]
          [else
        (values (field 'slides)
                (let ([p (field 'program)]) (and (pair? p) (car p)))
                (let ([p (field 'deck)]) (and (pair? p) (car p))))])]
       [else (values #f #f #f)])]))
