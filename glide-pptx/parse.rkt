#lang racket/base
;; pptx -> deck IR.
;;
;; The package is expanded into `workdir` and stays there, because the IR refers
;; to media by package-relative part name and the renderer has to be able to
;; open those files.
(require racket/list racket/string racket/file racket/path
         "xml-util.rkt" "units.rkt" "ir.rkt" "opc.rkt" "theme.rkt"
         "drawing.rkt" "text.rkt" "shapes.rkt")
(provide pptx->deck current-warnings current-allow-unsupported?)

;; Collected diagnostics for things we render approximately.
(define current-warnings (make-parameter #f))

(define (warn! msg)
  (define bbox* (current-warnings))
  (when bbox* (set-box! bbox* (cons msg (unbox bbox*)))))

(define TYPE/SLIDE "/slide")
(define TYPE/LAYOUT "/slideLayout")
(define TYPE/MASTER "/slideMaster")
(define TYPE/THEME "/theme")

(define (pptx->deck pptx-path #:workdir workdir)
  (make-directory* workdir)
  (define pkg (open-package pptx-path #:into workdir))
  (define pres-name (main-document-name pkg))
  (define pres (part-xexpr pkg pres-name))
  (define sz (child pres 'sldSz))
  (define width (or (and sz (string->emu-pt (attr sz 'cx))) 720.0))
  (define height (or (and sz (string->emu-pt (attr sz 'cy))) 540.0))
  (define default-text-style (child pres 'defaultTextStyle))
  ;; <p:sldId> carries both an unprefixed id and the r:id that names the
  ;; relationship, so the relationship has to be read as the namespaced one.
  (define slide-names
    (for/list ([sid (in-list (xpath* pres 'sldIdLst 'sldId))])
      (define rid (attr-ns sid 'id))
      (and rid (rel-target pkg pres-name rid))))
  (define slides
    (for/list ([name (in-list (filter values slide-names))] [i (in-naturals 1)])
      (parse-slide pkg name i width height default-text-style)))
  (deck width height slides workdir (path->string (simplify-path pptx-path))))

;; The package root's relationships name the main document part; for a
;; presentation that is ppt/presentation.xml, but the name is not guaranteed.
(define (main-document-name pkg)
  (define targets (rel-targets-by-type pkg "" "/officeDocument"))
  (if (pair? targets) (first targets) "ppt/presentation.xml"))

;; ------------------------------------------------------------------- slides

(define (single-rel pkg from suffix)
  (define ts (rel-targets-by-type pkg from suffix))
  (and (pair? ts) (first ts)))

(define (parse-slide pkg slide-name index width height default-text-style)
  (define slide-x (part-xexpr pkg slide-name))
  (define layout-name (single-rel pkg slide-name TYPE/LAYOUT))
  (define layout-x (and layout-name (part-xexpr pkg layout-name)))
  (define master-name (and layout-name (single-rel pkg layout-name TYPE/MASTER)))
  (define master-x (and master-name (part-xexpr pkg master-name)))
  (define theme-name (and master-name (single-rel pkg master-name TYPE/THEME)))
  (define th (if theme-name (parse-theme (part-xexpr pkg theme-name)) (empty-theme)))

  ;; The master's clrMap names which scheme slot "tx1" and friends mean; a slide
  ;; or layout may override it.
  (define clr-map (merge-clr-map master-x layout-x slide-x))
  (define cctx (clr-ctx th clr-map #f))

  (define (sp-tree-of x) (and x (xpath x 'cSld 'spTree)))
  (define layout-tree (sp-tree-of layout-x))
  (define master-tree (sp-tree-of master-x))
  (define layout-phs (if layout-tree (collect-phs layout-tree) '()))
  (define master-phs (if master-tree (collect-phs master-tree) '()))

  (define (media-for part) (lambda (rid) (rel-target pkg part rid)))

  (define (ctx-for part layout-phs master-phs tx-styles)
    (make-shape-ctx #:clr-ctx cctx #:theme th
                    #:layout-phs layout-phs #:master-phs master-phs
                    #:tx-styles tx-styles #:default-text-style default-text-style
                    #:media (media-for part) #:warn warn!))

  (define tx-styles (and master-x (child master-x 'txStyles)))

  ;; PowerPoint paints the master's and layout's own graphics behind the slide,
  ;; unless the slide turns that off. Their placeholders are not painted: those
  ;; only appear where the slide fills them in.
  (define show-master? (string->bool (attr slide-x 'showMasterSp) #t))
  (define background-elements
    (if show-master?
        (append
         (if master-tree
             (parse-sp-tree (ctx-for master-name '() master-phs tx-styles) master-tree
                            #:skip-placeholders? #t)
             '())
         (if layout-tree
             (parse-sp-tree (ctx-for layout-name layout-phs master-phs tx-styles) layout-tree
                            #:skip-placeholders? #t)
             '()))
        '()))

  (define slide-tree (sp-tree-of slide-x))
  ;; Filled in as the tree is read, with the names that came from our alt text.
  (define tag-names (make-hash))
  (define elements
    (parameterize ([current-tag-names tag-names])
      (parse-sp-tree (ctx-for slide-name layout-phs master-phs tx-styles) slide-tree)))

  (define bg (resolve-background cctx (media-for slide-name)
                                 (list slide-x layout-x master-x)))
  (slide index
         (or (slide-display-name slide-x) (format "Slide ~a" index))
         width height bg
         background-elements
         ;; A name is the element's key for export and for merging edits back,
         ;; so it has to be unique within its slide -- which PowerPoint does not
         ;; guarantee. Doing it here means every path downstream gets a key,
         ;; whether it goes through generated code or renders directly.
         ;;
         ;; A name that came from our own alt text is exempt: several shapes
         ;; carrying one tag were drawn by one `at`, and renaming them apart
         ;; would turn one code site into three.
         (uniquify-names elements (hash-keys tag-names))))

(define (uniquify-names elements [exempt '()])
  (define seen (make-hash))
  (define (rename e)
    (define name (element-name e))
    (define renamed
      (cond
        [(string=? "" name) e]
        [(member name exempt) e]
        [else
         (define n (hash-ref seen name 0))
         (hash-set! seen name (add1 n))
         (if (zero? n) e (element-with-name e (format "~a (~a)" name (add1 n))))]))
    (if (group? renamed)
        (group (element-id renamed) (element-name renamed) (element-bbox renamed)
               (map rename (group-children renamed)) (group-child-bbox renamed))
        renamed))
  (map rename elements))

(define (slide-display-name slide-x)
  (define cs (child slide-x 'cSld))
  (define n (and cs (attr cs 'name)))
  (and n (not (string=? n "")) n))

(define (collect-phs sp-tree)
  (for/list ([sp (in-list (elem-children sp-tree))]
             #:when (memq (local-name (car sp)) '(sp pic graphicFrame)))
    (define-values (type idx) (placeholder-info sp))
    (and type (list type idx sp))))

(define clr-map-keys
  '(bg1 tx1 bg2 tx2 accent1 accent2 accent3 accent4 accent5 accent6 hlink folHlink))

(define (merge-clr-map master-x layout-x slide-x)
  (define base
    (let ([m (and master-x (child master-x 'clrMap))])
      (if m
          (for/hash ([k (in-list clr-map-keys)])
            (values k (string->symbol (or (attr m k) (symbol->string k)))))
          (hash))))
  ;; An override replaces the whole map, so take the innermost one that has it.
  (define (override-of x)
    (define o (and x (child x 'clrMapOvr)))
    (define m (and o (child o 'overrideClrMapping)))
    (and m (for/hash ([k (in-list clr-map-keys)])
             (values k (string->symbol (or (attr m k) (symbol->string k)))))))
  (or (override-of slide-x) (override-of layout-x) base))

;; The first of slide/layout/master that states a background.
(define (resolve-background cctx media candidates)
  (for/or ([x (in-list candidates)])
    (define bg (and x (xpath x 'cSld 'bg)))
    (cond
      [(not bg) #f]
      [(child bg 'bgPr) => (lambda (p) (let ([f (parse-fill cctx p #:media media)])
                                         (and (not (eq? 'inherit f)) f)))]
      ;; <p:bgRef> selects an entry of the theme's background fill list.
      [(child bg 'bgRef)
       => (lambda (r)
            (define idx (or (attr-num r 'idx) 0))
            (define styles (theme-bg-fill-styles (clr-ctx-theme cctx)))
            (and (>= idx 1) (<= idx (length styles))
                 (parse-fill-element
                  (struct-copy clr-ctx cctx [ph-color (resolve-color-child cctx r)])
                  (list-ref styles (sub1 idx)) #:media media)))]
      [else #f])))
