#lang racket/base
;; Two-way sync between a Pict program and a .pptx.
;;
;; Editing the program and editing the deck alternate, so both sides drift
;; between syncs. That is a three-way merge against a base -- the state both
;; sides agreed on last time -- not two one-way converters.
;;
;; What makes it stable is an ownership rule that falls out of the intent:
;; PowerPoint owns geometry, because that is what you opened it to change, and
;; code owns everything else. With the two sides owning disjoint properties the
;; ordinary cycle has no conflicts at all.
;;
;; Nothing here ever restructures the program. Every edit is one of two things:
;; replace a numeric or string literal, or wrap an expression in a known form.
(require racket/list racket/string racket/format racket/file racket/path
         racket/math racket/port pict
         (only-in shrubbery/parse parse-all)
         "ir.rkt" "draw-ir.rkt" "parse.rkt" "semantic.rkt" "sync-state.rkt"
         (only-in "runtime.rkt" current-media-base))
(provide (struct-out sync-action) (struct-out sync-report)
         program-slide-states deck-slide-states
         match-elements merge-states
         apply-actions! sync-once
         base-path-for format-sync-report)

;; kind: 'moved, 'resized, 'retext, 'conflict, 'added, 'removed, 'unpatchable
(struct sync-action (kind tag slide detail) #:transparent)
(struct sync-report (actions applied base-written?) #:transparent)

;; ------------------------------------------------------- reading both sides

;; A program's state comes from running it, so it reflects the code as edited
;; rather than whatever it was generated from.
;;
;; The load has to happen in a fresh namespace. A sync patches the source and
;; then reads it again to record the new agreed state, and `dynamic-require`
;; hands back the module instance it already has -- so without this the second
;; read returns the state from before the patch and the sync never converges.
;; The runtime and `pict` are attached rather than re-instantiated, so the picts
;; that come back are the same struct types this module knows, and the media
;; base is the same parameter.
(define (program-slide-states program-path)
  (define full (path->complete-path program-path))
  (define ns (make-base-empty-namespace))
  (for ([m (in-list '(pict glide-pptx/runtime glide-pptx/tagged glide-pptx/ir))])
    (namespace-attach-module (current-namespace) m ns))
  (define picts
    (parameterize ([current-media-base (path-only full)])
      ;; `all_slides` is the Rhombus spelling of the same convention.
      (define v (parameterize ([current-namespace ns])
                  (for/or ([name (in-list '(all-slides all_slides))])
                    (dynamic-require `(file ,(path->string full)) name
                                     (lambda () #f)))))
      (cond
        [(list? v) v]
        [(and v (vector? v)) (vector->list v)]
        [v (with-handlers ([exn:fail? (lambda (_e) (list v))])
             ((dynamic-require 'racket/treelist 'treelist->list) v))]
        [else (error 'sync "~a provides no all-slides" program-path)])))
  (for/list ([p (in-list picts)] [i (in-naturals 1)])
    (define w (pict-width p)) (define h (pict-height p))
    (items->slide-state i w h (display-page-items (pict->page p w h)))))

(define (deck-slide-states pptx-path #:workdir [workdir #f])
  (define dir (or workdir (make-temporary-file "syncdeck~a" 'directory)))
  (deck->slide-states (pptx->deck pptx-path #:workdir dir)))

;; ------------------------------------------------------------------ matching

;; Matches `now` against `base` within one slide. Tags come first, because they
;; are exact when the editor preserved them; a signature match picks up the rest
;; and is what makes this work when they were stripped.
;;
;; Returns (values pairs unmatched-now unmatched-base), pairs as (now . base).
(define MATCH-LIMIT 8.0)

(define (match-elements now base #:slide-size [size 1000.0])
  (define by-tag (for/hash ([b (in-list base)] #:when (el-state-tag b))
                   (values (el-state-tag b) b)))
  (define used (make-hasheq))
  (define pairs
    (filter values
            (for/list ([n (in-list now)])
              (define hit (and (el-state-tag n) (hash-ref by-tag (el-state-tag n) #f)))
              (cond
                [(and hit (not (hash-ref used hit #f))) (hash-set! used hit #t) (cons n hit)]
                [else #f]))))
  (define matched-now (for/hasheq ([p (in-list pairs)]) (values (car p) #t)))
  (define rest-now (filter (lambda (n) (not (hash-ref matched-now n #f))) now))
  (define rest-base (filter (lambda (b) (not (hash-ref used b #f))) base))
  ;; Everything left is matched by how much it looks alike, best pair first.
  (define costs
    (sort (for*/list ([n (in-list rest-now)] [b (in-list rest-base)]
                      #:when (< (signature-distance n b #:slide-size size) MATCH-LIMIT))
            (list (signature-distance n b #:slide-size size) n b))
          < #:key first))
  (define extra
    (filter values
            (for/list ([c (in-list costs)])
              (define n (second c)) (define b (third c))
              (cond
                [(or (hash-ref matched-now n #f) (hash-ref used b #f)) #f]
                [else (hash-set! matched-now n #t) (hash-set! used b #t) (cons n b)]))))
  (values (append pairs extra)
          (filter (lambda (n) (not (hash-ref matched-now n #f))) now)
          (filter (lambda (b) (not (hash-ref used b #f))) base)))

;; ------------------------------------------------------------------- merging

;; Three-way merge for one slide. `base` is the agreed state, `prog` the program
;; as it is now, `deck` the .pptx as it is now.
(define (merge-slide index base prog deck size)
  (define-values (deck-pairs deck-added deck-removed)
    (match-elements deck base #:slide-size size))
  (define prog-by-tag (for/hash ([p (in-list prog)] #:when (el-state-tag p))
                        (values (el-state-tag p) p)))
  (append
   (append*
    (for/list ([pair (in-list deck-pairs)])
      (define d (car pair)) (define b (cdr pair))
      (define tag (or (el-state-tag b) (el-state-tag d)))
      (define p (and tag (hash-ref prog-by-tag tag #f)))
      (define deck-moved? (not (el-geometry-same? d b)))
      (define prog-moved? (and p (not (el-geometry-same? p b))))
      (define deck-retext? (not (string=? (el-state-text d) (el-state-text b))))
      (define prog-retext? (and p (not (string=? (el-state-text p) (el-state-text b)))))
      (filter
       values
       (list
        (cond
          ;; Both sides moved it. PowerPoint wins, because dragging is why it
          ;; was opened -- but say so rather than doing it quietly.
          [(and deck-moved? prog-moved?)
           (sync-action 'conflict tag index
                        (list 'geometry (el-geometry b) (el-geometry p) (el-geometry d)))]
          [deck-moved?
           (sync-action (if (and (< (abs (- (el-state-w d) (el-state-w b))) 0.05)
                                 (< (abs (- (el-state-h d) (el-state-h b))) 0.05))
                            'moved 'resized)
                        tag index (el-geometry d))]
          [else #f])
        (cond
          ;; Text is the code's, so a program edit wins and a deck-only edit is
          ;; taken.
          [(and deck-retext? prog-retext?)
           (sync-action 'conflict tag index
                        (list 'text (el-state-text b) (el-state-text p) (el-state-text d)))]
          [deck-retext? (sync-action 'retext tag index (el-state-text d))]
          [else #f])))))
   (for/list ([d (in-list deck-added)])
     (sync-action 'added (or (el-state-tag d) "(unnamed)") index (el-geometry d)))
   (for/list ([b (in-list deck-removed)])
     (sync-action 'removed (or (el-state-tag b) "(unnamed)") index (el-geometry b)))))

(define (merge-states base prog deck)
  (append*
   (for/list ([bs (in-list base)])
     (define index (slide-state-index bs))
     (define (slide-for l) (for/first ([s (in-list l)]
                                       #:when (= index (slide-state-index s))) s))
     (define ps (slide-for prog))
     (define ds (slide-for deck))
     (cond
       [(and ps ds)
        (merge-slide index (slide-state-elements bs) (slide-state-elements ps)
                     (slide-state-elements ds)
                     (max 1.0 (slide-state-width bs)))]
       [else (list (sync-action 'conflict (format "slide ~a" index) index
                                '(slide-missing)))]))))

;; -------------------------------------------------------- patching the source

;; One `(at x y ... #:tag "T" ... child)` form, with the source ranges of the
;; literals a merge is allowed to replace. A range is #f when the value is not a
;; literal -- `(at margin (+ top 20) ...)` is a decision the code is making, and
;; is reported rather than overwritten.
(struct at-site (tag x y rot width height texts) #:transparent)
(struct rng (start end) #:transparent)

;; Shifts every recovered range, for a reader that was handed only part of the
;; file. The shrubbery parser cannot see a `#lang` line, so a Rhombus program is
;; parsed from just after it and the ranges are moved back into the whole file.
(define current-range-offset (make-parameter 0))

(define (range-of stx)
  (and (syntax-position stx) (syntax-span stx)
       (let ([start (+ (current-range-offset) (sub1 (syntax-position stx)))])
         (rng start (+ start (syntax-span stx))))))

(define (literal-range stx pred)
  (and stx (pred (syntax-e stx)) (range-of stx)))

;; Reads the program fresh, so the ranges reflect the file as it is now rather
;; than as it was generated. That is what survives the user reformatting it: the
;; syntax tree is read to *find* things, and only the literals themselves are
;; overwritten, so no formatting is ever regenerated.
(define (find-at-sites path)
  (if (regexp-match? #rx"[.]rhm$" (if (path? path) (path->string path) path))
      (rhombus-at-sites path)
      (racket-at-sites path)))

(define (racket-at-sites path)
  (define forms
    (call-with-input-file path
      (lambda (in)
        (port-count-lines! in)
        ;; The program starts with `#lang`, so the reader has to be enabled.
        (parameterize ([read-accept-reader #t] [read-accept-lang #t])
          (let loop ([acc '()])
            (define s (read-syntax path in))
            (if (eof-object? s) (reverse acc) (loop (cons s acc))))))))
  (define sites '())
  (let walk ([stx (datum->syntax #f forms)])
    (define l (syntax->list stx))
    (when l
      (when (and (pair? l) (eq? 'at (syntax-e (car l))))
        (define site (parse-at-form (cdr l)))
        (when site (set! sites (cons site sites))))
      (for-each walk l)))
  (reverse sites))

;; ---------------------------------------------------------------- rhombus

;; Shrubbery parses `at(57.6, 158.4, ~tag: "Box", shape_pict(...))` as
;;
;;   (group at (parens (group 57.6) (group 158.4)
;;                     (group #:tag (block (group "Box")))
;;                     (group shape_pict (parens ...))))
;;
;; so a call is an identifier followed by a parens group, a positional argument
;; is a one-element group, and a keyword argument is a group whose head is the
;; keyword and whose value sits in a block.
(define (rhombus-at-sites path)
  (define text (file->string path))
  ;; `parse-all` reads shrubbery, not a module, so the `#lang` line comes off
  ;; first and its length is added back to every range.
  (define after-lang
    (cond
      [(regexp-match-positions #rx"^#lang[^\n]*\n" text) => (lambda (m) (cdar m))]
      [else 0]))
  (define stx
    (let ([in (open-input-string (substring text after-lang))])
      (port-count-lines! in)
      (parse-all in #:source path)))
  (define sites '())
  ;; The offset has to be in effect while the ranges are computed, which is
  ;; during the walk.
  (parameterize ([current-range-offset after-lang])
    (let walk ([s stx])
      (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
      (when l
        (define call (rhombus-call l))
        (when (and call (eq? 'at (car call)))
          (define site (parse-rhombus-at (cdr call)))
          (when site (set! sites (cons site sites))))
        (for-each walk l))
      (void)))
  (reverse sites))

;; (cons name arg-groups) when `l` is a call, else #f.
(define (rhombus-call l)
  (and (>= (length l) 3)
       (eq? 'group (syntax-e* (first l)))
       (symbol? (syntax-e* (second l)))
       (let ([p (third l)])
         (and (rhombus-head? p 'parens)
              (cons (syntax-e* (second l)) (cdr (syntax-e p)))))))

(define (syntax-e* s) (if (syntax? s) (syntax-e s) s))

(define (rhombus-head? s tag)
  (and (syntax? s)
       (let ([e (syntax-e s)])
         (and (list? e) (pair? e) (eq? tag (syntax-e* (car e)))))))

;; The single expression a group holds, or #f when it holds more than one.
(define (rhombus-group-value g)
  (define e (and (syntax? g) (syntax-e g)))
  (and (list? e) (= 2 (length e)) (eq? 'group (syntax-e* (first e))) (second e)))

;; A keyword argument's value, which shrubbery wraps in a block.
(define (rhombus-kw-value g)
  (define e (and (syntax? g) (syntax-e g)))
  (and (list? e) (= 3 (length e)) (eq? 'group (syntax-e* (first e)))
       (keyword? (syntax-e* (second e)))
       (let ([b (third e)])
         (and (rhombus-head? b 'block)
              (let ([inner (cdr (syntax-e b))])
                (and (= 1 (length inner)) (rhombus-group-value (car inner))))))))

(define (rhombus-kw-name g)
  (define e (and (syntax? g) (syntax-e g)))
  (and (list? e) (>= (length e) 2) (eq? 'group (syntax-e* (first e)))
       (keyword? (syntax-e* (second e)))
       (syntax-e* (second e))))

(define (parse-rhombus-at args)
  ;; A positional argument is any group that is not a keyword argument. Note it
  ;; can hold more than one term: the last one is a call, so its group is
  ;; `(group shape_pict (parens ...))`.
  (define positional (filter (lambda (g) (not (rhombus-kw-name g))) args))
  (define kws (for/hash ([g (in-list args)] #:when (rhombus-kw-name g))
                (values (rhombus-kw-name g) (rhombus-kw-value g))))
  (define tag-stx (hash-ref kws '#:tag #f))
  (define tag (and tag-stx (string? (syntax-e* tag-stx)) (syntax-e* tag-stx)))
  (and tag (>= (length positional) 3)
       (let ([child (last positional)])
         (at-site tag
                  (literal-range (rhombus-group-value (first positional)) real?)
                  (literal-range (rhombus-group-value (second positional)) real?)
                  (literal-range (hash-ref kws '#:rotate #f) real?)
                  (rhombus-child-kw child '#:width)
                  (rhombus-child-kw child '#:height)
                  (rhombus-child-texts child)))))

;; The child of an `at` is a group holding one call, so its keyword arguments
;; are found the same way.
(define (rhombus-child-kw child kw)
  (define l (and (syntax? child) (syntax-e child)))
  (define parens (and (list? l) (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (literal-range (rhombus-kw-value g) real?)))))

(define (rhombus-child-texts child)
  (define acc '())
  (let walk ([s child])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when l
      (define call (rhombus-call l))
      (when (and call (memq (car call) '(run run_star)))
        (define first-arg (and (pair? (cdr call)) (rhombus-group-value (second call))))
        (when (and first-arg (string? (syntax-e* first-arg)))
          (set! acc (cons (range-of first-arg) acc))))
      (for-each walk l)))
  (reverse (filter values acc)))

(define (parse-at-form args)
  (define positional '())
  (define kws (make-hash))
  (let loop ([as args])
    (cond
      [(null? as) (void)]
      [(keyword? (syntax-e (car as)))
       (when (pair? (cdr as)) (hash-set! kws (syntax-e (car as)) (cadr as)))
       (loop (if (pair? (cdr as)) (cddr as) '()))]
      [else (set! positional (cons (car as) positional)) (loop (cdr as))]))
  (define ps (reverse positional))
  (define tag-stx (hash-ref kws '#:tag #f))
  (define tag (and tag-stx (string? (syntax-e tag-stx)) (syntax-e tag-stx)))
  (and tag (>= (length ps) 3)
       (let ([child (last ps)])
         (at-site tag
                  (literal-range (first ps) real?)
                  (literal-range (second ps) real?)
                  (literal-range (hash-ref kws '#:rotate #f) real?)
                  (child-kw-range child '#:width)
                  (child-kw-range child '#:height)
                  (child-text-ranges child)))))

;; A leaf's size lives on the leaf, not on `at`.
(define (child-kw-range child kw)
  (define l (syntax->list child))
  (and l
       (let loop ([as l])
         (cond
           [(or (null? as) (null? (cdr as))) #f]
           [(eq? kw (syntax-e (car as))) (literal-range (cadr as) real?)]
           [else (loop (cdr as))]))))

;; Every string literal that is the first argument of a `run*`, in order, which
;; is where a text edit has to land.
(define (child-text-ranges child)
  (define acc '())
  (let walk ([stx child])
    (define l (syntax->list stx))
    (when l
      (when (and (pair? l) (memq (syntax-e (car l)) '(run* run))
                 (pair? (cdr l)) (string? (syntax-e (cadr l))))
        (set! acc (cons (range-of (cadr l)) acc)))
      (for-each walk l)))
  (reverse (filter values acc)))

;; --------------------------------------------------------------- applying

;; Applies the actions a merge produced, editing only literals. Returns
;; (values applied skipped).
(define (apply-actions! program-path actions)
  (define sites (for/hash ([s (in-list (find-at-sites program-path))])
                  (values (at-site-tag s) s)))
  (define edits '())
  (define applied '())
  (define skipped '())
  (define (edit! r text) (when r (set! edits (cons (list r text) edits))))
  (for ([a (in-list actions)])
    (define site (hash-ref sites (sync-action-tag a) #f))
    (case (sync-action-kind a)
      [(moved resized)
       (define g (sync-action-detail a))
       (define-values (x y w h rot) (apply values g))
       (cond
         [(not site) (set! skipped (cons (cons a "no tagged `at` form in the source") skipped))]
         [(not (and (at-site-x site) (at-site-y site)))
          (set! skipped (cons (cons a "its position is computed, not a literal") skipped))]
         [else
          (edit! (at-site-x site) (num->source x))
          (edit! (at-site-y site) (num->source y))
          (when (at-site-rot site) (edit! (at-site-rot site) (num->source rot)))
          (cond
            [(eq? 'resized (sync-action-kind a))
             (cond
               [(and (at-site-width site) (at-site-height site))
                (edit! (at-site-width site) (num->source w))
                (edit! (at-site-height site) (num->source h))
                (set! applied (cons a applied))]
               [else
                (set! skipped (cons (cons a "its size is computed, not a literal") skipped))])]
            [else (set! applied (cons a applied))])])]
      [(retext)
       (define texts (and site (at-site-texts site)))
       (cond
         [(not site) (set! skipped (cons (cons a "no tagged `at` form in the source") skipped))]
         [(not (= 1 (length texts)))
          (set! skipped (cons (cons a (format "its text spans ~a runs; edit it by hand"
                                              (length texts)))
                              skipped))]
         [else (edit! (first texts) (format "~s" (sync-action-detail a)))
               (set! applied (cons a applied))])]
      [else (set! skipped (cons (cons a "reported only") skipped))]))
  (when (pair? edits) (splice-file! program-path edits))
  (values (reverse applied) (reverse skipped)))

;; A number as it should read in source: the same rounding the emitter uses.
(define (num->source v)
  (define r (/ (round (* 1000.0 (exact->inexact v))) 1000.0))
  (if (integer? r) (format "~a.0" (inexact->exact r)) (format "~a" r)))

;; Replaces ranges from the end backwards, so earlier offsets stay valid.
;; Comments, formatting and every untouched line survive exactly.
;;
;; The work happens in characters, not bytes: `syntax-position` counts
;; characters, and a generated program has multi-byte ones in it -- a bullet
;; glyph is three bytes and one character, so splicing by byte offset lands in
;; the wrong place further down the file.
(define (splice-file! path edits)
  (define text (file->string path))
  (define sorted (sort edits > #:key (lambda (e) (rng-start (first e)))))
  (define out
    (for/fold ([text text]) ([e (in-list sorted)])
      (define r (first e))
      (string-append (substring text 0 (rng-start r))
                     (second e)
                     (substring text (rng-end r)))))
  (call-with-output-file path #:exists 'replace (lambda (o) (write-string out o))))

;; ------------------------------------------------------------------- driver

(define (base-path-for program-path)
  (path-replace-extension (path->complete-path program-path) ".sync.rktd"))

;; One merge pass: read both sides, merge against the base, patch the source,
;; and record the new agreed state.
(define (sync-once program-path pptx-path
                   #:workdir [workdir #f]
                   #:dry-run? [dry-run? #f])
  (define base-file (base-path-for program-path))
  (define-values (base _p _d) (read-sync-base base-file))
  (define prog (program-slide-states program-path))
  (define deck (deck-slide-states pptx-path #:workdir workdir))
  (cond
    ;; With no base there is nothing to merge against: the program is the truth
    ;; and this pass just records where both sides stand.
    [(not base)
     (unless dry-run?
       (write-sync-base base-file prog
                        #:program (path->string (path->complete-path program-path))
                        #:deck (path->string (path->complete-path pptx-path))))
     (sync-report '() '() (not dry-run?))]
    [else
     (define actions (merge-states base prog deck))
     (cond
       [dry-run? (sync-report actions '() #f)]
       [else
        (define-values (applied skipped) (apply-actions! program-path actions))
        ;; The new base is the program as it now reads, so the next pass
        ;; compares against something both sides agree on.
        (define after (program-slide-states program-path))
        (write-sync-base base-file after
                         #:program (path->string (path->complete-path program-path))
                         #:deck (path->string (path->complete-path pptx-path)))
        (sync-report actions applied #t)])]))

(define (format-sync-report r)
  (define o (open-output-string))
  (define as (sync-report-actions r))
  (cond
    [(null? as) (fprintf o "  nothing to merge\n")]
    [else
     (for ([a (in-list as)])
       (fprintf o "  slide ~a  ~a  ~s~a\n" (sync-action-slide a)
                (~a (sync-action-kind a) #:min-width 12)
                (sync-action-tag a)
                (if (memq (sync-action-kind a) '(moved resized))
                    (let ([g (sync-action-detail a)])
                      (format "  -> ~a,~a ~ax~a"
                              (~r (first g) #:precision 1) (~r (second g) #:precision 1)
                              (~r (third g) #:precision 1) (~r (fourth g) #:precision 1)))
                    "")))
     (fprintf o "  ~a applied, ~a reported\n"
              (length (sync-report-applied r))
              (- (length as) (length (sync-report-applied r))))])
  (get-output-string o))
