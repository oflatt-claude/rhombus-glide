#lang racket/base
;; Text body parsing, including the property inheritance PowerPoint performs.
;;
;; A run's font size can be stated on the run, on its paragraph's defRPr, on the
;; shape's lstStyle, on the matching placeholder in the slide layout, on the
;; same placeholder in the slide master, in the master's txStyles for that
;; placeholder kind, or in the presentation's defaultTextStyle -- and in real
;; decks it usually is not on the run. `text-ctx` carries that chain, ordered
;; most specific first, and every property lookup walks it.
(require racket/list racket/string
         "xml-util.rkt" "units.rkt" "ir.rkt" "theme.rkt" "drawing.rkt")
(provide (struct-out text-ctx)
         parse-text-body parse-body-pr
         resolve-run-props)

;; lvl-sources are <a:lstStyle>-shaped nodes holding lvl1pPr..lvl9pPr children.
;; body-prs are <a:bodyPr> nodes, same ordering.
(struct text-ctx (clr-ctx theme lvl-sources body-prs))

(define (lvl-node src lvl)
  (and src (child src (string->symbol (format "lvl~apPr" (add1 (max 0 (min 8 lvl))))))))

;; --------------------------------------------------------------- chain walks

;; First value of attribute `name` along `chain`, or #f.
(define (chain-attr chain name)
  (for/first ([n (in-list chain)] #:when (and n (attr n name))) (attr n name)))

;; First node in `chain` that has a child named `name`, then that child.
(define (chain-child chain name)
  (for/first ([n (in-list chain)] #:when (and n (child n name))) (child n name)))

;; First node in `chain` that states a fill at all, so an explicit <a:noFill>
;; on a specific node wins over a color further up.
(define (chain-fill tctx chain)
  (define holder
    (for/first ([n (in-list chain)]
                #:when (and n (for/or ([c (in-list (elem-children n))])
                                (memq (local-name (car c)) '(noFill solidFill gradFill
                                                             blipFill pattFill)))))
      n))
  (and holder (parse-fill (text-ctx-clr-ctx tctx) holder)))

;; ------------------------------------------------------------------ bodyPr

;; Body-level properties, inherited across the shape/layout/master bodyPr chain.
(define (parse-body-pr tctx #:default-anchor [default-anchor 'top])
  (define chain (filter values (text-ctx-body-prs tctx)))
  (define (num name default)
    (define v (chain-attr chain name))
    (if v (or (string->emu-pt v) default) default))
  (values
   (case (chain-attr chain 'anchor)
     [("t") 'top] [("ctr") 'center] [("b") 'bottom]
     [("just" "dist") 'top] [else default-anchor])
   (string->bool (chain-attr chain 'anchorCtr) #f)
   (not (equal? "none" (chain-attr chain 'wrap)))
   (cond [(chain-child chain 'normAutofit) 'shrink]
         [(chain-child chain 'spAutoFit) 'grow]
         [else 'none])
   (insets (num 'lIns 7.2) (num 'tIns 3.6) (num 'rIns 7.2) (num 'bIns 3.6))
   (or (string->angle (chain-attr chain 'rot)) 0.0)
   ;; A <a:normAutofit fontScale> is PowerPoint's cached shrink factor; honoring
   ;; it matches what the file was last seen to look like.
   (let ([na (chain-child chain 'normAutofit)])
     (cons (or (and na (string->percent (attr na 'fontScale))) 1.0)
           (or (and na (string->percent (attr na 'lnSpcReduction))) 0.0)))))

;; ------------------------------------------------------------------- runs

;; Resolves one run's character properties. `rPr` may be #f (a run that states
;; nothing, or an empty paragraph's endParaRPr).
(define (resolve-run-props tctx rPr pPr lvl)
  (define lvl-defs
    (for/list ([s (in-list (text-ctx-lvl-sources tctx))])
      (let ([n (lvl-node s lvl)]) (and n (child n 'defRPr)))))
  (define chain (filter values (append (list rPr (and pPr (child pPr 'defRPr))) lvl-defs)))
  (define size (let ([v (chain-attr chain 'sz)])
                 (if v (hundredths->pt (or (string->number v) 1800)) 18.0)))
  (define family
    (or (resolve-typeface (text-ctx-theme tctx)
                          (let ([l (chain-child chain 'latin)]) (and l (attr l 'typeface))))
        (theme-latin-font (text-ctx-theme tctx) 'minor)
        "sans-serif"))
  (define fill (chain-fill tctx chain))
  (define color (cond [(solid-fill? fill) (solid-fill-color fill)]
                      [(gradient-fill? fill) (cdr (first (gradient-fill-stops fill)))]
                      [else black]))
  (values size family
          (string->bool (chain-attr chain 'b) #f)
          (string->bool (chain-attr chain 'i) #f)
          (let ([u (chain-attr chain 'u)]) (and u (not (string=? u "none"))))
          (let ([s (chain-attr chain 'strike)]) (and s (not (string=? s "noStrike"))))
          color
          (let ([v (chain-attr chain 'spc)])
            (if v (hundredths->pt (or (string->number v) 0)) 0.0))
          (case (chain-attr chain 'cap) [("all") 'all] [("small") 'small] [else 'none])
          (or (string->percent (chain-attr chain 'baseline)) 0.0)))

;; --------------------------------------------------------------- paragraphs

(define (align-of s)
  (case s
    [("l") 'left] [("ctr") 'center] [("r") 'right]
    [("just" "justLow") 'justify] [("dist" "thaiDist") 'justify]
    [else #f]))

(define (parse-spacing node)
  (cond
    [(not node) #f]
    [(child node 'spcPct) => (lambda (n) (cons 'percent (or (string->percent (attr n 'val)) 1.0)))]
    [(child node 'spcPts) => (lambda (n) (cons 'points (hundredths->pt (or (attr-num n 'val) 0))))]
    [else #f]))

(define (parse-bullet tctx chain lvl)
  (cond
    [(chain-child chain 'buNone) no-bullet]
    [(chain-child chain 'buChar)
     => (lambda (n)
          (bullet 'char (or (attr n 'char) "•")
                  (bullet-font tctx chain) (bullet-size chain) (bullet-color tctx chain)))]
    [(chain-child chain 'buAutoNum)
     => (lambda (n)
          (bullet 'number (or (attr n 'type) "arabicPeriod")
                  (bullet-font tctx chain) (bullet-size chain) (bullet-color tctx chain)))]
    [else no-bullet]))

(define (bullet-font tctx chain)
  (define f (chain-child chain 'buFont))
  (and f (resolve-typeface (text-ctx-theme tctx) (attr f 'typeface))))

(define (bullet-size chain)
  (define n (chain-child chain 'buSzPct))
  (or (and n (string->percent (attr n 'val))) 1.0))

(define (bullet-color tctx chain)
  (define n (chain-child chain 'buClr))
  (and n (resolve-color-child (text-ctx-clr-ctx tctx) n)))

(define (parse-paragraph tctx p)
  (define pPr (child p 'pPr))
  (define lvl (or (and pPr (attr-num pPr 'lvl)) 0))
  (define chain (filter values (cons pPr (for/list ([s (in-list (text-ctx-lvl-sources tctx))])
                                           (lvl-node s lvl)))))
  (define runs
    (for/list ([c (in-list (elem-children p))]
               #:when (memq (local-name (car c)) '(r br fld)))
      (define rPr (child c 'rPr))
      (define text (case (local-name (car c))
                     [(br) "\n"]
                     ;; A field renders its cached <a:t>; slide numbers and dates
                     ;; are the common cases and their cache is what was shown.
                     [else (all-text (or (child c 't) c))]))
      (define-values (size family bold? italic? underline? strike? color spc caps base)
        (resolve-run-props tctx rPr pPr lvl))
      (trun text family size bold? italic? underline? strike? color spc caps base)))
  ;; An empty paragraph still occupies a line, whose height comes from
  ;; endParaRPr; keep a zero-width run so the layout reserves it.
  (define runs*
    (if (null? runs)
        (let-values ([(size family bold? italic? underline? strike? color spc caps base)
                      (resolve-run-props tctx (child p 'endParaRPr) pPr lvl)])
          (list (trun "" family size bold? italic? underline? strike? color spc caps base)))
        runs))
  (para runs*
        (or (align-of (chain-attr chain 'algn)) 'left)
        lvl
        (let ([v (chain-attr chain 'marL)]) (if v (or (string->emu-pt v) 0.0) 0.0))
        (let ([v (chain-attr chain 'indent)]) (if v (or (string->emu-pt v) 0.0) 0.0))
        (or (parse-spacing (chain-child chain 'lnSpc)) (cons 'percent 1.0))
        (let ([s (parse-spacing (chain-child chain 'spcBef))])
          (spacing->points s 0.0 (largest-size runs*)))
        (let ([s (parse-spacing (chain-child chain 'spcAft))])
          (spacing->points s 0.0 (largest-size runs*)))
        (parse-bullet tctx chain lvl)))

(define (largest-size runs)
  (if (null? runs) 18.0 (apply max (map trun-size runs))))

;; spcBef/spcAft as a percentage is a percentage of the line's font size.
(define (spacing->points s default font-size)
  (cond
    [(not s) default]
    [(eq? 'points (car s)) (cdr s)]
    [else (* (cdr s) font-size)]))

;; --------------------------------------------------------------- text body

(define (parse-text-body tctx tx-body #:default-anchor [default-anchor 'top])
  (cond
    [(not tx-body) #f]
    [else
     (define-values (anchor anchor-ctr? wrap? autofit ins rot scale) 
       (parse-body-pr tctx #:default-anchor default-anchor))
     (define paras (for/list ([p (in-list (children tx-body 'p))]) (parse-paragraph tctx p)))
     ;; Fold PowerPoint's cached autofit scale into the sizes, so the IR needs
     ;; no notion of "shrunk to fit" downstream.
     (define font-scale (car scale))
     (define paras*
       (if (= 1.0 font-scale)
           paras
           (for/list ([p (in-list paras)])
             (struct-copy para p
                          [runs (for/list ([r (in-list (para-runs p))])
                                  (struct-copy trun r [size (* font-scale (trun-size r))]))]))))
     (text-body paras* anchor anchor-ctr? wrap? autofit ins rot)]))
