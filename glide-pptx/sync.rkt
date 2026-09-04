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
         racket/math racket/port racket/treelist pict
         (only-in shrubbery/parse parse-all)
         "ir.rkt" "draw-ir.rkt" "parse.rkt" "semantic.rkt" "sync-state.rkt"
         (only-in "runtime.rkt" current-media-base)
         (only-in "emit-common.rkt" media-names-for dominant-font)
         (only-in "emit-rhombus.rkt" rhombus-element-source rhombus-slide-source))
(provide (struct-out sync-action) (struct-out sync-report)
         program-slide-states deck-slide-states load-program-picts
         match-elements merge-states
         apply-actions! sync-once
         find-at-sites find-program-sites
         (struct-out at-site) (struct-out slide-site) (struct-out rng)
         (struct-out program-layout) (struct-out name-list) (struct-out name-entry)
         base-path-for format-sync-report current-keep-work?)

;; Whether scratch directories are left behind for inspection.
(define current-keep-work? (make-parameter #f))

;; kind: 'moved, 'resized, 'retext, 'restyle, 'restacked, 'reordered,
;; 'conflict, 'added, 'removed, 'added-slide, 'ambiguous
;; `prior` is where the program currently draws the element, which is what a
;; correction for a computed position is measured against. It is #f when the
;; program has no element by that name.
(struct sync-action (kind tag slide detail prior) #:transparent)
;; `skipped` pairs each action the merge could not make with the reason. Knowing
;; an edit was refused, and why, is the whole difference between "nothing moved"
;; and "your drag was thrown away".
(struct sync-report (actions applied skipped base-written?) #:transparent)

;; ------------------------------------------------------- reading both sides

;; Loading a program's slides, which three callers need and each got slightly
;; wrong on its own: the binding is `all_slides` in Rhombus and `all-slides` in
;; Racket, and a Rhombus `List` is a treelist rather than a list.
;;
;; The load happens in a fresh namespace. A sync patches the source and then
;; reads it again to record the new agreed state, and `dynamic-require` hands
;; back the module instance it already has -- so without this the second read
;; returns the state from before the patch and the sync never converges. The
;; runtime and `pict` are attached rather than re-instantiated, so the picts that
;; come back are the same struct types this module knows, and the media base is
;; the same parameter.
(define (load-program-picts program-path #:named [named #f])
  (define full (path->complete-path program-path))
  (define ns (make-base-empty-namespace))
  (for ([m (in-list '(pict glide-pptx/runtime glide-pptx/tagged glide-pptx/ir))])
    (namespace-attach-module (current-namespace) m ns))
  (define names (if named (list (string->symbol named)) '(all_slides all-slides)))
  (define found
    (parameterize ([current-media-base (path-only full)] [current-namespace ns])
      (for/or ([name (in-list names)])
        (dynamic-require `(file ,(path->string full)) name (lambda () #f)))))
  (cond
    [(list? found) found]
    [(treelist? found) (treelist->list found)]
    [(pict? found) (list found)]
    [else
     (error 'glide
            (string-append "~a provides no list of slide picts.\n"
                           "  Expected a provided `all_slides` (or `all-slides`),"
                           " or a name passed with --slides.")
            program-path)]))

(define (program-slide-states program-path)
  (for/list ([p (in-list (load-program-picts program-path))] [i (in-naturals 1)])
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
  ;; By tag, several deep: one `at` in a loop draws several elements under one
  ;; tag, so a tag can name a family on both sides. Families are paired in
  ;; z-order, which is the order the code drew them in.
  (define (by-tag l)
    (for/fold ([h (hash)] #:result (for/hash ([(k v) (in-hash h)])
                                    (values k (sort (reverse v) < #:key el-state-z))))
              ([e (in-list l)] #:when (el-state-tag e))
      (hash-update h (el-state-tag e) (lambda (v) (cons e v)) '())))
  (define base-by-tag (by-tag base))
  (define used (make-hasheq))
  (define matched-now (make-hasheq))
  (define pairs
    (append*
     (for/list ([(tag ns) (in-hash (by-tag now))])
       (define bs (hash-ref base-by-tag tag '()))
       ;; Only when the family is the same size on both sides. If one member was
       ;; deleted, pairing by position would slide every survivor onto the wrong
       ;; base element and read as a move -- so the whole family goes to the
       ;; signature matcher, which pairs each survivor with itself and leaves the
       ;; missing one to be reported as missing.
       (cond
         [(= (length ns) (length bs))
          (for/list ([n (in-list ns)] [b (in-list bs)])
            (hash-set! used b #t)
            (hash-set! matched-now n #t)
            (cons n b))]
         [else '()]))))
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

;; Which named properties differ, as (name was now). A property missing on one
;; side is not a difference: a deck states a font where a program leaves it to
;; the theme, and neither is an edit.
;; Stands for a property one side does not have at all, so that it is told apart
;; from a `bold` that is really false.
(define ABSENT 'absent)

;; What the two states disagree about, as (property was now). An outline the
;; editor added, or took away, is as much of a change as one it recoloured, so
;; a property missing from either side is reported with `ABSENT` rather than
;; passed over.
(define (style-changes was now)
  (define (value l k) (let ([p (assq k l)]) (if p (cdr p) ABSENT)))
  (define keys (remove-duplicates (append (map car was) (map car now))))
  (filter values
          (for/list ([k (in-list keys)])
            (define a (value was k))
            (define b (value now k))
            (and (not (equal? a b)) (list k a b)))))

;; A tag names one *code site*, and one `at` inside a loop draws several
;; elements. So a tag can stand for a family, and an edit to a family only
;; makes sense when it is an edit to all of it:
;;
;;   dragged all of them the same way  ->  one edit, on the one `at`
;;   dragged one of them               ->  refused; the loop computes the rest
;;   deleted one of them               ->  refused; a loop cannot say "but not
;;                                          that one"
;;
;; The refusal is the point. Guessing would silently move the other two.
(define FAMILY-EPSILON 0.05)

(define (same-delta? a b)
  (and (< (abs (- (first a) (first b))) FAMILY-EPSILON)
       (< (abs (- (second a) (second b))) FAMILY-EPSILON)))

(define (geometry-delta d b)
  (list (- (el-state-x d) (el-state-x b)) (- (el-state-y d) (el-state-y b))))

;; Three-way merge for one slide. `base` is the agreed state, `prog` the program
;; as it is now, `deck` the .pptx as it is now.
(define (merge-slide index base prog deck size)
  (define-values (deck-pairs deck-added deck-removed)
    (match-elements deck base #:slide-size size))
  ;; By tag, not one per tag: a tag can name several elements from one `at`.
  ;; In z-order, which is the order the code drew them -- a family's members are
  ;; compared position by position, so both sides have to agree on which is
  ;; which.
  (define (by-tag l)
    (for/fold ([h (hash)] #:result (for/hash ([(k v) (in-hash h)])
                                     (values k (sort (reverse v) < #:key el-state-z))))
              ([e (in-list l)] #:when (el-state-tag e))
      (hash-update h (el-state-tag e) (lambda (v) (cons e v)) '())))
  (define prog-by-tag (by-tag prog))
  (define (prog-count tag) (length (hash-ref prog-by-tag tag '())))
  ;; Pairs grouped by tag, so a family is decided once rather than per element.
  ;; In the order the tags first appear, so the report reads down the slide.
  (define groups
    (let loop ([ps deck-pairs] [order '()] [h (hash)])
      (cond
        [(null? ps) (for/list ([tag (in-list (reverse order))])
                      (list tag (reverse (hash-ref h tag))))]
        [else
         (define pair (car ps))
         (define tag (or (el-state-tag (cdr pair)) (el-state-tag (car pair))))
         (loop (cdr ps)
               (if (hash-has-key? h tag) order (cons tag order))
               (hash-update h tag (lambda (v) (cons pair v)) '()))])))
  (append
   (append*
    (for/list ([g (in-list groups)])
      (define tag (first g))
      (define pairs (second g))
      (if (or (> (length pairs) 1) (> (prog-count tag) 1))
          (family-actions tag index pairs (hash-ref prog-by-tag tag '()))
          (single-actions tag index (first pairs)
                          (let ([ps (hash-ref prog-by-tag tag '())])
                            (and (pair? ps) (first ps)))))))
   (for/list ([d (in-list deck-added)])
     (sync-action 'added (or (el-state-tag d) "(unnamed)") index (el-geometry d) #f))
   (for/list ([b (in-list deck-removed)])
     (define tag (el-state-tag b))
     ;; One of a family deleted: the others are still drawn by the same `at`.
     (if (and tag (> (prog-count tag) 1))
         (sync-action 'ambiguous tag index
                      (format "~a elements share this tag, and deleting one of them is not something the code can say"
                              (prog-count tag))
                      #f)
         (sync-action 'removed (or tag "(unnamed)") index (el-geometry b) #f)))))

;; Every element under one tag, which one `at` drew.
(define (family-actions tag index pairs prog-elements)
  (define moved
    (for/list ([pair (in-list pairs)]
               #:when (not (el-geometry-same? (car pair) (cdr pair))))
      pair))
  (cond
    [(null? moved) '()]
    ;; Some moved and some did not, or they moved differently: there is no one
    ;; correction that produces this.
    [(not (and (= (length moved) (length pairs))
               (let ([d0 (geometry-delta (car (first moved)) (cdr (first moved)))])
                 (for/and ([pair (in-list (cdr moved))])
                   (same-delta? d0 (geometry-delta (car pair) (cdr pair)))))))
     (list (sync-action 'ambiguous tag index
                        (format "~a elements share this tag and they did not all move the same way"
                                (length pairs))
                        #f))]
    [else
     ;; All of them, by the same amount. That is one correction on the one `at`.
     (define d (car (first moved)))
     (define b (cdr (first moved)))
     (define p (and (pair? prog-elements) (first prog-elements)))
     (list (sync-action 'moved tag index (el-geometry d) (and p (el-geometry p))))]))

;; The ordinary case: one element, one tag, one `at`.
(define (single-actions tag index pair p)
  (define d (car pair)) (define b (cdr pair))
  (define deck-moved? (not (el-geometry-same? d b)))
  (define prog-moved? (and p (not (el-geometry-same? p b))))
  (define deck-retext? (not (string=? (el-state-text d) (el-state-text b))))
  (define prog-retext? (and p (not (string=? (el-state-text p) (el-state-text b)))))
  (filter
   values
   (list
    (cond
      ;; Both sides moved it. PowerPoint wins, because dragging is why it was
      ;; opened -- but say so rather than doing it quietly.
      [(and deck-moved? prog-moved?)
       (sync-action 'conflict tag index
                    (list 'geometry (el-geometry b) (el-geometry p) (el-geometry d))
                    (and p (el-geometry p)))]
      [deck-moved?
       (sync-action (if (and (< (abs (- (el-state-w d) (el-state-w b))) 0.05)
                             (< (abs (- (el-state-h d) (el-state-h b))) 0.05))
                        'moved 'resized)
                    tag index (el-geometry d)
                    (and p (el-geometry p)))]
      [else #f])
    ;; Bringing a shape to the front changes the order it is drawn in and
    ;; nothing else. Rewriting that means moving the `at` form itself, which is
    ;; a restructuring rather than a literal edit -- so it is reported and left,
    ;; which is better than the silence it used to get.
    (and (not (= (el-state-z b) (el-state-z d)))
         (sync-action 'restacked tag index
                      (format "it is drawn ~a now, not ~a; move its `at` form to change that"
                              (add1 (el-state-z d)) (add1 (el-state-z b)))
                      #f))
    ;; What the editor changed about the look of it. Appearance is the code's, so
    ;; this is reported and written only where the source holds a literal -- but
    ;; reported it must be: recolouring a shape in the editor used to disappear
    ;; without a word.
    (let ([changes (style-changes (el-state-style b) (el-state-style d))])
      (and (pair? changes) (sync-action 'restyle tag index changes #f)))
    (cond
      ;; Text is the code's, so a program edit wins and a deck-only edit is
      ;; taken.
      [(and deck-retext? prog-retext?)
       (sync-action 'conflict tag index
                    (list 'text (el-state-text b) (el-state-text p) (el-state-text d))
                    (and p (el-geometry p)))]
      [deck-retext? (sync-action 'retext tag index (el-state-text d) #f)]
      [else #f]))))

;; Which deck slide is which of the base's. The merge pairs slides so that
;; adding one in the editor does not shift every later one: it used to pair by
;; index, and a slide pasted at the front made every following slide compare
;; against its neighbour -- 28 "edits" and eight elements deleted.
;;
;; Slides are matched on their elements' tags, which is a strong fingerprint: an
;; untouched slide keeps all of them, and a slide pasted in from another deck
;; shares none. Best pair first, one base slide to one deck slide, so a
;; duplicated slide claims its original once and the copy is left over as new.
(define SLIDE-MATCH 0.5)

(define (slide-affinity a b)
  (define ta (filter values (map el-state-tag (slide-state-elements a))))
  (define tb (filter values (map el-state-tag (slide-state-elements b))))
  (cond
    [(and (null? ta) (null? tb)) 1.0]
    [(or (null? ta) (null? tb)) 0.0]
    [else
     (define-values (shared _left)
       (for/fold ([n 0] [h (for/fold ([h (hash)]) ([t (in-list tb)])
                             (hash-update h t add1 0))])
                 ([t (in-list ta)])
         (if (positive? (hash-ref h t 0))
             (values (add1 n) (hash-update h t sub1))
             (values n h))))
     (/ (* 2.0 shared) (+ (length ta) (length tb)))]))

;; (values pairs added removed), pairs as (deck . base), in deck order.
(define (match-slides base deck)
  (define costs
    (sort (for*/list ([d (in-list deck)] [b (in-list base)])
            (list (slide-affinity d b) d b))
          > #:key first))
  (define used-d (make-hasheq))
  (define used-b (make-hasheq))
  (for ([c (in-list costs)])
    (when (and (>= (first c) SLIDE-MATCH)
               (not (hash-ref used-d (second c) #f))
               (not (hash-ref used-b (third c) #f)))
      (hash-set! used-d (second c) (third c))
      (hash-set! used-b (third c) #t)))
  (values (for/list ([d (in-list deck)] #:when (hash-ref used-d d #f))
            (cons d (hash-ref used-d d)))
          (for/list ([d (in-list deck)] #:unless (hash-ref used-d d #f)) d)
          (for/list ([b (in-list base)] #:unless (hash-ref used-b b #f)) b)))

;; Even with every slide matched, a slide can have been swapped for a different
;; one that happens to look alike. A slide whose elements mostly do not match is
;; not the slide the base recorded, and calling the difference an edit would
;; delete the program's real elements.
(define WHOLESALE 0.5)

(define (wholesale-change? base-elements actions)
  (define n (length base-elements))
  (define gone (for/sum ([a (in-list actions)]
                         #:when (memq (sync-action-kind a) '(removed))) 1))
  (and (> n 1) (> gone (* WHOLESALE n))))

;; A slide deleted in the editor is not merged back yet, and taking the
;; difference for edits would delete the program's real elements.
(define (check-slides-removed removed program-path)
  (unless (null? removed)
    (error 'glide
           (string-append
            "~a slide~a in ~a ~a not in the deck any more.\n"
            "  Deleting a slide in the editor is not merged back yet: the program\n"
            "  says which slides exist. Remove its `def slide_N` and its entry in\n"
            "  `all_slides`, and export again.")
           (length removed) (if (= 1 (length removed)) "" "s")
           (if (path? program-path) (file-name-from-path program-path) program-path)
           (if (= 1 (length removed)) "is" "are"))))

(define (merge-states base prog deck [program-path "the program"])
  (define-values (pairs added removed) (match-slides base deck))
  (check-slides-removed removed program-path)
  (append
   (append*
    (for/list ([pair (in-list pairs)])
      (define ds (car pair))
      (define bs (cdr pair))
      (define index (slide-state-index bs))
      (define ps (for/first ([s (in-list prog)]
                             #:when (= index (slide-state-index s))) s))
      (cond
        [ps
         (define as (merge-slide index (slide-state-elements bs) (slide-state-elements ps)
                                 (slide-state-elements ds)
                                 (max 1.0 (slide-state-width bs))))
         (if (wholesale-change? (slide-state-elements bs) as)
             (list (sync-action
                    'ambiguous (format "slide ~a" index) index
                    (format (string-append
                             "most of this slide's ~a elements are not in the deck any more,"
                             " so it is a different slide rather than an edited one")
                            (length (slide-state-elements bs)))
                    #f))
             as)]
        [else (list (sync-action 'conflict (format "slide ~a" index) index
                                 '(slide-missing) #f))])))
   ;; Dragging slides about in the navigator changes their order and nothing
   ;; else, so no element differs and the merge used to see nothing at all.
   ;; `all_slides` is a literal list, which is exactly what says the order.
   (let ([order (for/list ([d (in-list (in-deck-order deck))])
                  (let ([b (for/first ([p (in-list pairs)] #:when (eq? d (car p))) (cdr p))])
                    (and b (slide-state-index b))))])
     (if (and (andmap values order)
              (not (equal? order (sort order <))))
         (list (sync-action 'reordered "the slides" 0 order #f))
         '()))
   ;; A slide the base does not have was added in the editor. Where it goes in
   ;; the program's order is where it sits in the deck: after whichever program
   ;; slide the nearest earlier deck slide belongs to.
   (let loop ([ds (in-deck-order deck)] [after 0] [seq 0] [acc '()])
     (cond
       [(null? ds) (reverse acc)]
       [else
        (define d (car ds))
        (define b (for/first ([p (in-list pairs)] #:when (eq? d (car p))) (cdr p)))
        (if b
            (loop (cdr ds) (slide-state-index b) 0 acc)
            (loop (cdr ds) after (add1 seq)
                  (cons (sync-action 'added-slide
                                     (format "deck slide ~a" (slide-state-index d))
                                     (slide-state-index d)
                                     (list after seq)
                                     #f)
                        acc)))]))))

(define (in-deck-order deck)
  (sort deck < #:key slide-state-index))

;; -------------------------------------------------------- patching the source

;; One `(at x y ... #:tag "T" ... child)` form, with the source ranges of the
;; literals a merge is allowed to replace. A range is #f when the value is not a
;; literal -- `(at margin (+ top 20) ...)` is a decision the code is making, and
;; is reported rather than overwritten.
;; `nudge` is (list range dx dy) for an existing `#:nudge` argument, and
;; `insert-at` is the position a new one would go, just after the tag.
;; A tag names one element *within one slide*. That is what makes the deck's own
;; shape names usable as tags: a real deck has "Title 1" on every slide. So each
;; site remembers the top-level definition it sits in, and `all-slides` says
;; which definition is which slide -- without that, dragging the title on slide 3
;; would rewrite slide 1's coordinates.
(struct at-site (tag x y rot width height texts nudge insert-at scope whole
                 flip-h flip-v leaf-at styles) #:transparent)

;; Where a new element goes in one slide's definition: just after the last
;; argument of its `slide-canvas` call, at that argument's indentation. Adding a
;; shape in the editor puts it on top, which is where the last argument draws.
(struct slide-site (scope insert-at indent def-end) #:transparent)
(struct rng (start end) #:transparent)

;; Shifts every recovered range, for a reader that was handed only part of the
;; file. The shrubbery parser cannot see a `#lang` line, so a Rhombus program is
;; parsed from just after it and the ranges are moved back into the whole file.
(define current-range-offset (make-parameter 0))

(define (range-of stx)
  (and (syntax-position stx) (syntax-span stx)
       (let ([start (+ (current-range-offset) (sub1 (syntax-position stx)))])
         (rng start (+ start (syntax-span stx))))))

;; Two elements answering to one tag would mean an edit lands on an arbitrary one
;; of them. That is refused rather than guessed at. The usual cause is an `at`
;; that runs more than once -- inside a `for`, or in a helper called twice --
;; which is fine code, just not code an editor's edit can be traced back to.
(define (duplicate-tags tags)
  (define counts
    (for/fold ([h (hash)]) ([t (in-list tags)] #:when t)
      (hash-update h t add1 0)))
  (sort (for/list ([(t n) (in-hash counts)] #:when (> n 1)) (cons t n))
        string<? #:key car))

(define (check-unique-tags tags where hint)
  (define dups (duplicate-tags tags))
  (unless (null? dups)
    (error 'glide
           "~a uses one tag for more than one element, so a sync cannot tell them apart:\n~a  ~a"
           where
           (apply string-append
                  (for/list ([d (in-list dups)])
                    (format "    ~s appears ~a times\n" (car d) (cdr d))))
           hint)))

(define TAG-HINT
  (string-append
   "Give the two `at` forms different tags. One `at` may carry a tag that repeats\n"
   "  -- a loop draws several elements from one site -- but two sites may not share one."))

;; -------------------------------------------------- delimiters, on the text

;; Shrubbery's group wrappers carry no source location -- only the leaf terms do
;; -- so the extent of a call has to be found in the text. Doing it this way for
;; both languages also means the result follows the file as the user reformats
;; it, rather than as it was generated.
;;
;; `line?` and `block?` are the two comment syntaxes: ";;" for Racket, "//" and
;; "/* */" for Rhombus.
(define (match-close text open line? block?)
  (define n (string-length text))
  (define (closer c) (case c [(#\() #\)] [(#\[) #\]] [(#\{) #\}] [else #f]))
  (let loop ([i open] [stack '()])
    (cond
      [(>= i n) #f]
      [else
       (define c (string-ref text i))
       (define (peek k) (and (< (+ i k) n) (string-ref text (+ i k))))
       (cond
         ;; A string literal can hold anything, including a lone paren.
         [(char=? c #\")
          (let skip ([j (add1 i)])
            (cond
              [(>= j n) #f]
              [(char=? (string-ref text j) #\\) (skip (+ j 2))]
              [(char=? (string-ref text j) #\") (loop (add1 j) stack)]
              [else (skip (add1 j))]))]
         [(and line? (line? c (peek 1)))
          (let skip ([j i])
            (cond [(>= j n) #f]
                  [(char=? (string-ref text j) #\newline) (loop (add1 j) stack)]
                  [else (skip (add1 j))]))]
         [(and block? (block? c (peek 1)))
          (let skip ([j (+ i 2)])
            (cond [(>= (add1 j) n) #f]
                  [(and (char=? (string-ref text j) #\*)
                        (char=? (string-ref text (add1 j)) #\/))
                   (loop (+ j 2) stack)]
                  [else (skip (add1 j))]))]
         [(closer c) (loop (add1 i) (cons (closer c) stack))]
         [(and (pair? stack) (char=? c (car stack)))
          (if (null? (cdr stack)) i (loop (add1 i) (cdr stack)))]
         [else (loop (add1 i) stack)])])))

(define (rhombus-close text open)
  (match-close text open
               (lambda (c d) (and (char=? c #\/) (eqv? d #\/)))
               (lambda (c d) (and (char=? c #\/) (eqv? d #\*)))))

;; The first opening paren at or after `i`.
(define (next-open text i)
  (define n (string-length text))
  (let loop ([j i])
    (cond [(>= j n) #f]
          [(char=? (string-ref text j) #\() j]
          [else (loop (add1 j))])))

;; Walks back over whitespace, which is where a new last argument belongs: after
;; the previous one, not after the newline that was indenting the closing paren.
(define (back-over-space text i)
  (let loop ([j i])
    (if (and (> j 0) (char-whitespace? (string-ref text (sub1 j)))) (loop (sub1 j)) j)))

;; The indentation of the line `i` sits on, which is the indentation a new
;; sibling should get.
(define (indent-at text i)
  (define start
    (let loop ([j (min i (sub1 (string-length text)))])
      (cond [(<= j 0) 0]
            [(char=? (string-ref text (sub1 j)) #\newline) j]
            [else (loop (sub1 j))])))
  (let loop ([j start] [k 0])
    (if (and (< j (string-length text)) (char=? (string-ref text j) #\space))
        (loop (add1 j) (add1 k))
        k)))

;; Where a new last argument goes in `call-name(...)`, given the position just
;; after the call's name.
;; A new element is indented like the last one in the same slide. The default
;; only applies to a slide that has none yet.
(define (with-indents slide-sites sites text)
  (for/list ([ss (in-list slide-sites)])
    (define mine (filter (lambda (s) (and (equal? (slide-site-scope ss) (at-site-scope s))
                                          (at-site-whole s)))
                         sites))
    (cond
      [(null? mine) ss]
      [else
       (define start (rng-start (at-site-whole (last mine))))
       (struct-copy slide-site ss [indent (indent-at text start)])])))

(define (canvas-insertion text after-name close)
  (define open (next-open text after-name))
  (define shut (and open (close text open)))
  (and shut
       (let ([at (back-over-space text shut)])
         (list at (indent-at text (sub1 at))))))

(define (literal-range stx pred)
  (and stx (pred (syntax-e stx)) (range-of stx)))

;; Reads the program fresh, so the ranges reflect the file as it is now rather
;; than as it was generated. That is what survives the user reformatting it: the
;; syntax tree is read to *find* things, and only the literals themselves are
;; overwritten, so no formatting is ever regenerated.
;; (values sites scopes). `scopes` names the definition that builds each slide,
;; in order, or is #f when `all-slides` is not a literal list of names: a program
;; that computes its slide list cannot have a slide traced back to source, and
;; then tags have to be unique across the whole file instead of per slide.
;; Only Rhombus source is patched. Any program that provides slide picts can be
;; rendered and exported -- that goes through `dynamic-require` and does not care
;; what language wrote it -- but tracing an edit back to a literal means reading
;; the source, and there is one reader.
(define (find-program-sites path)
  (define s (if (path? path) (path->string path) path))
  (unless (regexp-match? #rx"[.]rhm$" s)
    (error 'glide
           (string-append "~a is not Rhombus source, so edits cannot be merged into it.\n"
                          "  Rendering and export work on any program; syncing needs a .rhm.")
           s))
  (rhombus-at-sites path))

(define (find-at-sites path)
  (define-values (sites _scopes _slides _layout) (find-program-sites path))
  sites)

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
  ;; `parse-all` returns `(multi group ...)`, so the top-level groups -- one per
  ;; `def` -- are the scopes.
  (define groups
    (let ([e (syntax-e stx)])
      (if (and (list? e) (eq? 'multi (syntax-e* (car e)))) (cdr e) (list stx))))
  ;; The offset has to be in effect while the ranges are computed, which is
  ;; during the walk.
  (parameterize ([current-range-offset after-lang] [current-source-text text])
    (for ([g (in-list groups)])
      (define scope (rhombus-def-name g))
      (let walk ([s g])
        (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
        (when l
          (define call (rhombus-call l))
          (when (and call (eq? 'at (car call)))
            (define site (parse-rhombus-at (cdr call)))
            (when site
              (set! sites (cons (struct-copy at-site site
                                             [scope scope]
                                             [whole (rhombus-call-extent text (second l))])
                                sites))))
          (for-each walk l))
        (void))))
  (parameterize ([current-range-offset after-lang])
    (values (reverse sites) (rhombus-slide-scopes groups)
            (with-indents (rhombus-slide-sites groups text) (reverse sites) text)
            (program-layout (rhombus-name-list groups text)
                            (rhombus-export-block groups text)
                            (rhombus-global-colours groups)))))

;; Where a slide can be added, and where its name has to be entered for the
;; addition to mean anything. A `def slide_N` nobody lists in `all_slides` is
;; dead code, so the two edits go together.
(struct program-layout (slide-list exports globals) #:transparent)

;; `def all_slides = [slide_1, slide_2]` -- the brackets, and each name in them.
(struct name-list (open close items) #:transparent)
(struct name-entry (name range) #:transparent)

;; The `[...]` of the `all_slides` definition.
(define (rhombus-name-list groups text)
  (for/or ([g (in-list groups)])
    (define l (let ([e (syntax-e g)]) (and (list? e) e)))
    (and l (= 5 (length l)) (eq? 'group (syntax-e* (first l)))
         (eq? 'def (syntax-e* (second l)))
         (memq (syntax-e* (third l)) '(all_slides all-slides))
         (let ([b (let ([e (syntax-e (fifth l))]) (and (list? e) e))])
           (and b (eq? 'brackets (syntax-e* (car b)))
                (let* ([items
                        (filter values
                                (for/list ([grp (in-list (cdr b))])
                                  (define v (rhombus-group-value grp))
                                  (and v (symbol? (syntax-e* v))
                                       (name-entry (syntax-e* v) (range-of v)))))]
                       ;; The brackets carry no position, so they are found from
                       ;; the first name -- or, for an empty list, from the `=`.
                       [anchor (if (pair? items)
                                   (rng-start (name-entry-range (first items)))
                                   (let ([r (range-of (fourth l))]) (and r (rng-end r))))]
                       [open (and anchor (prev-char text anchor #\[))]
                       [close (and open (match-close text open
                                                     (lambda (c d)
                                                       (and (char=? c #\/) (eqv? d #\/)))
                                                     (lambda (c d)
                                                       (and (char=? c #\/) (eqv? d #\*)))))])
                  (and open close (name-list open close items))))))))

;; The first `ch` at or before `i`.
(define (prev-char text i ch)
  (let loop ([j (min i (sub1 (string-length text)))])
    (cond [(< j 0) #f]
          [(char=? (string-ref text j) ch) j]
          [else (loop (sub1 j))])))

;; The `export:` block, as the position a new name goes after and the
;; indentation it takes. A program need not have one -- `all_slides` is what an
;; export reads -- so this is #f when there is none.
(define (rhombus-export-block groups text)
  (for/or ([g (in-list groups)])
    (define l (let ([e (syntax-e g)]) (and (list? e) e)))
    (and l (= 3 (length l)) (eq? 'group (syntax-e* (first l)))
         (eq? 'export (syntax-e* (second l)))
         (let ([b (let ([e (syntax-e (third l))]) (and (list? e) e))])
           (and b (eq? 'block (syntax-e* (car b)))
                (let ([rs (filter values
                                  (for/list ([grp (in-list (cdr b))])
                                    (define v (rhombus-group-value grp))
                                    (and v (range-of v))))])
                  (and (pair? rs)
                       (let ([last-r (argmax rng-end rs)])
                         (cons (rng-end last-r)
                               (indent-at text (rng-start last-r)))))))))))

;; The `slide_canvas(...)` call in each `def slide_N = slide_canvas(...)`.
;; From the head of a call to just past its closing paren.
(define (rhombus-call-extent text name)
  (define r (range-of name))
  (define open (and r (next-open text (rng-end r))))
  (define shut (and open (rhombus-close text open)))
  (and shut (rng (rng-start r) (add1 shut))))

(define (rhombus-slide-sites groups text)
  (filter values
          (for/list ([g (in-list groups)])
            (define scope (rhombus-def-name g))
            (define l (let ([e (syntax-e g)]) (and (list? e) e)))
            (define name (and l (= 6 (length l))
                              (eq? 'slide_canvas (syntax-e* (fifth l)))
                              (fifth l)))
            (and scope name
                 (let* ([r (range-of name)]
                        [open (and r (next-open text (rng-end r)))]
                        [shut (and open (rhombus-close text open))]
                        [ins (and r (canvas-insertion text (rng-end r) rhombus-close))])
                   (and ins shut (slide-site scope (first ins) 2 (add1 shut))))))))

;; `(group def slide_1 (op =) ...)` -> slide_1, and the same for `fun`.
(define (rhombus-def-name g)
  (define l (let ([e (syntax-e g)]) (and (list? e) e)))
  (and l (>= (length l) 3) (eq? 'group (syntax-e* (first l)))
       (memq (syntax-e* (second l)) '(def fun))
       (symbol? (syntax-e* (third l)))
       (syntax-e* (third l))))

;; `def brand = hex("4472C4")` -> brand -> the range of that string. A colour
;; given a name is shared, so recolouring one shape that uses it is not the same
;; edit as recolouring the name.
(define (rhombus-global-colours groups)
  (for/fold ([h (hash)]) ([g (in-list groups)])
    (define l (let ([e (syntax-e g)]) (and (list? e) e)))
    (define nm (and l (>= (length l) 5) (eq? 'def (syntax-e* (second l)))
                    (symbol? (syntax-e* (third l)))
                    (syntax-e* (third l))))
    (define hit (and nm (hex-site g)))
    (if (and hit (eq? 'literal (car hit)))
        (hash-set h nm (cdr hit))
        h)))

;; `def all_slides = [slide_1, slide_2]` -> '(slide_1 slide_2).
(define (rhombus-slide-scopes groups)
  (for/or ([g (in-list groups)])
    (define l (let ([e (syntax-e g)]) (and (list? e) e)))
    (and l (= 5 (length l)) (eq? 'group (syntax-e* (first l)))
         (eq? 'def (syntax-e* (second l)))
         (memq (syntax-e* (third l)) '(all_slides all-slides))
         (let ([b (let ([e (syntax-e (fifth l))]) (and (list? e) e))])
           (and b (eq? 'brackets (syntax-e* (car b)))
                (let ([names (map (lambda (grp)
                                    (let ([v (rhombus-group-value grp)])
                                      (and v (symbol? (syntax-e* v)) (syntax-e* v))))
                                  (cdr b))])
                  (and (pair? names) (andmap values names) names)))))))

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
                  (rhombus-child-texts child)
                  (rhombus-nudge (hash-ref kws '#:nudge #f))
                  (let ([r (range-of tag-stx)]) (and r (rng-end r)))
                  #f #f
                  (rhombus-child-flag child '#:flip_h)
                  (rhombus-child-flag child '#:flip_v)
                  (rhombus-child-insert child)
                  (style-sites child)))))

;; `~nudge: [12.0, -4.0]` -> (list range dx dy).
;; The text being read, so an extent that syntax cannot give can be found in it.
(define current-source-text (make-parameter ""))

(define (rhombus-bracket-extent stx)
  (define text (current-source-text))
  (define kw (range-of stx))
  ;; `stx` is the brackets wrapper and its elements are groups, neither of which
  ;; has a position -- only the leaf inside the first group does, and the `[` is
  ;; the first one before it.
  (define inner
    (let ([e (and (syntax? stx) (syntax-e stx))])
      (and (list? e) (pair? (cdr e))
           (let ([v (rhombus-group-value (second e))])
             (and v (range-of v))))))
  (cond
    [(and (not kw) inner)
     (define open
       (let loop ([j (sub1 (rng-start inner))])
         (cond [(< j 0) #f]
               [(char=? (string-ref text j) #\[) j]
               [else (loop (sub1 j))])))
     (define shut (and open (match-close text open
                                         (lambda (c d) (and (char=? c #\/) (eqv? d #\/)))
                                         (lambda (c d) (and (char=? c #\/) (eqv? d #\*))))))
     (and shut (rng open (add1 shut)))]
    [else kw]))

(define (rhombus-nudge stx)
  (define e (and stx (syntax? stx) (syntax-e stx)))
  (and (list? e) (eq? 'brackets (syntax-e* (car e)))
       (let ([gs (map rhombus-group-value (cdr e))])
         (and (= 2 (length gs)) (andmap values gs)
              (real? (syntax-e* (first gs))) (real? (syntax-e* (second gs)))
              (list (rhombus-bracket-extent stx)
                    (syntax-e* (first gs)) (syntax-e* (second gs)))))))

;; The child of an `at` is a group holding one call, so its keyword arguments
;; are found the same way.
(define (rhombus-child-kw child kw)
  (define l (and (syntax? child) (syntax-e child)))
  (define parens (and (list? l) (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (literal-range (rhombus-kw-value g) real?)))))

;; The whole group a keyword introduces, rather than the one term inside it: a
;; `~fill: hex("4472C4")` is a call, not a value.
(define (rhombus-kw-group g)
  (define l (and (syntax? g) (let ([e (syntax-e g)]) (and (list? e) e))))
  (and l (>= (length l) 3) (eq? 'group (syntax-e* (first l)))
       (rhombus-head? (third l) 'block)
       (let ([b (syntax-e (third l))])
         (and (pair? (cdr b)) (second b)))))

;; Every `run(...)` call in a leaf, so a body of one run can have its typeface
;; and size rewritten and a body of several can be told apart from it.
(define (rhombus-para-calls child) (rhombus-calls-named child '(para para*)))

(define (rhombus-run-calls child) (rhombus-calls-named child '(run run*)))

(define (rhombus-calls-named child names)
  (define acc '())
  (let walk ([s child])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when l
      (define call (rhombus-call l))
      (when (and call (memq (car call) names))
        (set! acc (cons s acc)))
      (for-each walk l)))
  (reverse acc))

;; A keyword's literal value inside a particular call.
(define (rhombus-child-kw-in call kw pred)
  (define l (and (syntax? call) (let ([e (syntax-e call)]) (and (list? e) e))))
  (parens-kw-in (and l (findf (lambda (x) (rhombus-head? x 'parens)) l)) kw pred))

;; The same, for an argument list already in hand.
(define (parens-kw-in parens kw pred)
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (literal-range (rhombus-kw-value g) pred)))))

(define (rhombus-child-flag-in call kw)
  (define l (and (syntax? call) (let ([e (syntax-e call)]) (and (list? e) e))))
  (define parens (and l (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g))
              (let ([v (rhombus-kw-value g)]) (and v (range-of v)))))))

;; Where a style property is written in the source, if it is written as a
;; literal at all. A `~fill: hex("4472C4")` can be rewritten; a
;; `~fill: brand_blue` names something shared, and changing that would recolour
;; every shape using it -- so it is reported with the name instead.
;; `range` is where the value is written, when it is written at all. `insert-at`
;; and `keyword` are how to add it when it is not: a line that is solid has no
;; `~dash:` to rewrite, and adding one is a better answer than reporting that it
;; is missing.
;; `whole` is the whole argument's extent, for a property that can be written as
;; absent: a shape whose fill the editor removed says `~fill: #false`, and there
;; is no literal inside the old `hex(...)` that means that.
(struct style-site (property range shared insert-at keyword whole) #:transparent)

;; The literal inside the first `hex("...")` under `stx`, or the name it is given
;; instead.
(define (hex-site stx)
  (define found (box #f))
  ;; A call is a name followed by parentheses, and the two are siblings wherever
  ;; they appear: `~fill: hex("4472C4")` puts them in a group of their own, and
  ;; `def brand = hex("4472C4")` puts them at the end of a longer one.
  (let walk ([s stx])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when (and l (not (unbox found)))
      (for ([a (in-list l)] [b (in-list (cdr l))])
        (when (and (not (unbox found))
                   (eq? 'hex (syntax-e* a))
                   (rhombus-head? b 'parens))
          (define args (cdr (syntax-e b)))
          (define v (and (pair? args) (rhombus-group-value (first args))))
          (when (and v (string? (syntax-e* v)))
            (set-box! found (cons 'literal (range-of v))))))
      (unless (unbox found) (for-each walk l))))
  (cond
    [(unbox found) => values]
    [else
     ;; Not a `hex(...)`: a bare name is something shared.
     (define nm (let ([l (and (syntax? stx) (let ([e (syntax-e stx)]) (and (list? e) e)))])
                  (and l (= 2 (length l)) (symbol? (syntax-e* (second l)))
                       (syntax-e* (second l)))))
     (and nm (cons 'shared nm))]))


;; The `hex(...)` call itself, when the colour is written as one: an `~alpha:`
;; it does not state is added just inside these parentheses.
(define (hex-call stx)
  (define found (box #f))
  (let walk ([s stx])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when (and l (not (unbox found)))
      (for ([a (in-list l)] [b (in-list (cdr l))])
        (when (and (not (unbox found)) (eq? 'hex (syntax-e* a)) (rhombus-head? b 'parens))
          (set-box! found (cons a b))))
      (unless (unbox found) (for-each walk l))))
  (unbox found))

;; A fill the colour cannot be read out of: a gradient's first stop is a
;; `hex(...)` too, and rewriting it would leave the fill a gradient.
(define (compound-fill? stx) (and (fill-call-named stx '(gradient_fill image_fill pattern_fill)) #t))

;; A gradient can at least be replaced whole -- by a colour, when that is what
;; the editor set. An image or a pattern fill cannot: the comparison does not
;; see one, so a change to a shape wearing one is reported rather than guessed.
(define (gradient-fill-source? stx) (and (fill-call-named stx '(gradient_fill)) #t))

(define (fill-call-named stx names)
  (let walk ([s stx])
    (define e (and (syntax? s) (syntax-e s)))
    (cond [(memq e names) #t]
          [(list? e) (for/or ([x (in-list e)]) (walk x))]
          [else #f])))

;; The same, as the single term a literal is written as: `kw-value-stx` answers
;; with the whole group, which is what a colour has to be walked for.
;; A quoted name, `#'center`, which is two terms: the quote and the name. The
;; group around them carries no position of its own, so the extent comes from
;; the two ends.
(define (quoted-range g)
  (define l (and (syntax? g) (let ([e (syntax-e g)]) (and (list? e) e))))
  (define terms (if (and (pair? l) (eq? 'group (syntax-e* (first l)))) (cdr l) (or l '())))
  (and (= 2 (length terms))
       (let ([a (range-of (first terms))] [b (range-of (second terms))])
         (and a b (rng (rng-start a) (rng-end b))))))

(define (kw-single-stx child kw)
  (define l (and (syntax? child) (syntax-e child)))
  (define parens (and (list? l) (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (rhombus-kw-value g)))))

(define (kw-value-stx child kw)
  (define l (and (syntax? child) (syntax-e child)))
  (define parens (and (list? l) (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (rhombus-kw-group g)))))

;; The properties this can find in one `at` form.
(define (style-sites child)
  (define fill-stx (kw-value-stx child '#:fill))
  ;; The whole value of a keyword argument, which is what has to be rewritten
  ;; when there is no single literal inside it that says the same thing: an
  ;; outline being taken away, or a line spacing that is a `pair(...)`.
  (define (value-extent stx)
    (define name (call-name stx))
    (and name (rhombus-call-extent (current-source-text) name)))
  ;; Stated, or not stated. What the source states is rewritten where it
  ;; stands; what it does not is added. Never both: a second `~width:` in one
  ;; call would not compile, so an argument that is there but not a literal is
  ;; reported instead.
  ;;
  ;; `how` says what the value looks like: a predicate for a literal, `'call`
  ;; for a value that is a call of its own, `'flag` for a boolean.
  (define (kw-site property call kw how)
    (and call
         (let ([g (kw-value-stx call kw)])
           (if g
               (style-site property
                           (case how
                             [(call) (value-extent g)]
                             [(quoted) (quoted-range g)]
                             [(flag) (literal-range (kw-single-stx call kw) boolean?)]
                             [else (literal-range (kw-single-stx call kw) how)])
                           #f #f kw #f)
               (style-site property #f #f (call-append-at call) kw #f)))))
  ;; The colour argument, however the source states it: a `hex(...)` to rewrite,
  ;; a shared name, `#false` for a shape that has none, or nothing at all -- in
  ;; which case the whole argument is added.
  (define (paint-site property kw stx)
    (define hit (and stx (not (compound-fill? stx)) (hex-site stx)))
    (cond
      [(and stx (gradient-fill-source? stx))
       (style-site property #f #f #f kw (value-extent stx))]
      [(and stx (compound-fill? stx)) #f]
      [hit (style-site property
                       (and (eq? 'literal (car hit)) (cdr hit))
                       (and (eq? 'shared (car hit)) (cdr hit))
                       #f #f (value-extent stx))]
      [(and stx (literal-range stx boolean?))
       => (lambda (r) (style-site property #f #f #f kw r))]
      [(not stx) (style-site property #f #f (call-append-at child) kw #f)]
      [else #f]))
  ;; A colour's alpha lives inside its own `hex(...)`, so a shape made
  ;; translucent in the editor is an `~alpha:` written or added there.
  (define opacity
    (and fill-stx (not (compound-fill? fill-stx)) (hex-call fill-stx)
         (kw-site 'fill-opacity fill-stx '#:alpha real?)))
  ;; The stroke is a call of its own, so its colour, width and dash sit inside
  ;; it.
  (define stroke-stx (kw-value-stx child '#:line))
  (define stroke (and stroke-stx (call-name stroke-stx) stroke-stx))
  ;; The call that carries the body's own properties: a `textbox(...)` is one,
  ;; and a shape's `~body: body(...)` is the other.
  (define body-call
    (let ([nm (call-name child)])
      (if (and nm (memq (syntax-e* nm) '(textbox text_box)))
          child
          (kw-value-stx child '#:body))))
  ;; One run and one paragraph stand for the body, the same way the state
  ;; reports them: an edit to one run of many is refused anyway.
  (define run (let ([rs (rhombus-run-calls child)]) (and (= 1 (length rs)) (first rs))))
  (define one-para
    (let ([ps (rhombus-para-calls child)]) (and (= 1 (length ps)) (first ps))))
  ;; A run's colour is a call too.
  (define text-colour
    (and run
         (let* ([v (kw-value-stx run '#:color)]
                [hit (and v (hex-site v))])
           (cond
             [hit (style-site 'text-color
                              (and (eq? 'literal (car hit)) (cdr hit))
                              (and (eq? 'shared (car hit)) (cdr hit))
                              #f #f #f)]
             ;; Text whose colour the source never states, recoloured in the
             ;; editor: the argument is added to the run.
             [(not v) (style-site 'text-color #f #f (call-append-at run) '#:color #f)]
             [else #f]))))
  (filter values
          (list (paint-site 'fill '#:fill fill-stx)
                opacity
                (paint-site 'line '#:line stroke-stx)
                text-colour
                (kw-site 'line-width stroke '#:width real?)
                (kw-site 'dash stroke '#:dash 'quoted)
                (kw-site 'size run '#:size real?)
                (kw-site 'font run '#:font string?)
                (kw-site 'bold run '#:bold 'flag)
                (kw-site 'italic run '#:italic 'flag)
                (kw-site 'align one-para '#:align 'quoted)
                (kw-site 'line-spacing one-para '#:line_spacing 'call)
                (kw-site 'space-before one-para '#:space_before real?)
                (kw-site 'space-after one-para '#:space_after real?)
                (kw-site 'anchor body-call '#:anchor 'quoted)
                (kw-site 'wrap body-call '#:wrap 'flag)
                (kw-site 'autofit body-call '#:autofit 'quoted)
                (kw-site 'insets body-call '#:insets 'call))))

;; A boolean keyword on the leaf, as (range . value) -- so a flip that is
;; already there can be set either way. `#:width` and friends carry a number and
;; go through `rhombus-child-kw`; a flip carries `#true` or `#false`.
(define (rhombus-child-flag child kw)
  (define l (and (syntax? child) (syntax-e child)))
  (define parens (and (list? l) (findf (lambda (x) (rhombus-head? x 'parens)) l)))
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g))
              (let ([v (rhombus-kw-value g)])
                (and v (range-of v)))))))

;; Just inside the leaf call's parentheses, which is where a keyword it does not
;; have yet can be added.
(define (rhombus-child-insert child) (call-insert-at child))

;; Just inside a call's parentheses, which is where an argument it does not have
;; yet can be added.
(define (call-insert-at stx)
  (define r (call-name-range stx))
  (define open (and r (next-open (current-source-text) (rng-end r))))
  (and open (add1 open)))

;; A call's head: the symbol whose next sibling is the argument list.
(define (call-name stx)
  (define l (and (syntax? stx) (let ([e (syntax-e stx)]) (and (list? e) e))))
  (for/or ([a (in-list (or l '()))] [b (in-list (cdr (or l '(1))))])
    (and (symbol? (syntax-e* a)) (rhombus-head? b 'parens) a)))

(define (call-name-range stx)
  (let ([n (call-name stx)]) (and n (range-of n))))

;; Where a new argument belongs: after the last keyword argument there is, which
;; is how a call is laid out to begin with -- options first, content after. So
;; `textbox(~width: 300.0, ~anchor: #'center, para(...))` rather than an anchor
;; trailing the paragraphs. A call stating no options takes it at the end, which
;; is where `hex("ED7D31", ~alpha: 0.5)` wants it anyway.
(define (call-append-at stx)
  (define parens (call-parens stx))
  (define last-kw
    (and parens
         (for/fold ([best #f]) ([g (in-list (cdr (syntax-e parens)))])
           (if (rhombus-kw-name g)
               (let ([e (group-end g)]) (if (and e (> e (or best 0))) e best))
               best))))
  (define name (call-name stx))
  (define r (and (not last-kw) name (rhombus-call-extent (current-source-text) name)))
  (cond
    [last-kw last-kw]
    [r (back-over-space (current-source-text) (sub1 (rng-end r)))]
    [else #f]))

;; A call's argument list.
(define (call-parens stx)
  (define l (and (syntax? stx) (let ([e (syntax-e stx)]) (and (list? e) e))))
  (and l (findf (lambda (x) (rhombus-head? x 'parens)) l)))

;; How far a group reaches. The group itself carries no position, but every term
;; in it does -- including the head of a nested `(...)`, which spans the lot.
(define (group-end stx)
  (let walk ([s stx] [best #f])
    (define r (and (syntax? s) (range-of s)))
    (define here (if (and r (> (rng-end r) (or best 0))) (rng-end r) best))
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (if l (for/fold ([b here]) ([x (in-list l)]) (walk x b)) here)))

;; The comma a new last argument needs, which is none when the call had none.
(define (argument-comma text at)
  (if (and (> at 0) (char=? #\( (string-ref text (sub1 at)))) "" ", "))

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

;; --------------------------------------------------------------- applying

;; Applies the actions a merge produced, editing only literals. Returns
;; (values applied skipped).
;; Deleting an element takes with it the comment line that introduces it and the
;; whitespace that separated it from its siblings -- and, in Rhombus, the comma
;; that joined it to them, so the argument list stays well formed.
(define (deletion-range text r)
  (define comment "//")
  (define (line-start i)
    (let loop ([j i])
      (cond [(<= j 0) 0]
            [(char=? (string-ref text (sub1 j)) #\newline) j]
            [else (loop (sub1 j))])))
  ;; Absorb whole lines above while they are blank or comments.
  (define start
    (let loop ([s (rng-start r)])
      (define ls (line-start s))
      (cond
        [(not (string=? "" (string-trim (substring text ls s)))) s]
        [(zero? ls) ls]
        [else
         (define prev-start (line-start (sub1 ls)))
         (define prev (string-trim (substring text prev-start (sub1 ls))))
         (if (or (string=? "" prev) (string-prefix? prev comment))
             (loop prev-start)
             ;; Take the newline that ended the line above, so no blank is left.
             (sub1 ls))])))
  (define before (back-over-space text start))
  (cond
    [(and (> before 0) (char=? (string-ref text (sub1 before)) #\,))
     (rng (sub1 before) (rng-end r))]
    [else
     (define after
       (let loop ([j (rng-end r)])
         (if (and (< j (string-length text)) (char-whitespace? (string-ref text j)))
             (loop (add1 j))
             j)))
     (if (and (< after (string-length text)) (char=? (string-ref text after) #\,))
         (rng start (add1 after))
         (rng start (rng-end r)))]))

;; Within one slide definition a tag has to be unique, because that is the only
;; thing an edit is matched on. Across slides it need not be: "Title 1" on every
;; slide is the normal case. When the slide list is computed there is no per-slide
;; scope to speak of, so uniqueness has to hold file-wide.
(define (check-site-tags sites scopes path)
  (define where (format "~a" (if (path? path) (path->string path) path)))
  (cond
    [scopes
     (for ([scope (in-list (remove-duplicates (map at-site-scope sites)))])
       (check-unique-tags (for/list ([s (in-list sites)]
                                    #:when (equal? scope (at-site-scope s)))
                            (at-site-tag s))
                          (if scope (format "~a: ~a" where scope) where)
                          TAG-HINT))]
    [else
     (check-unique-tags (map at-site-tag sites)
                        (format "~a (its slide list is computed, so tags have to be unique file-wide)"
                                where)
                        TAG-HINT)]))

;; An element added in the editor has to be written as source, which is the same
;; job the translator does -- so it is the same code, for one element, at the
;; indentation its new siblings sit at.
(define (added-element d index tag)
  (and d
       (let ([s (for/first ([s (in-list (deck-slides d))]
                            #:when (= index (slide-index s)))
                  s)])
         (and s (let loop ([es (slide-elements s)])
                  (for/or ([e (in-list es)])
                    (cond
                      [(group? e) (loop (group-children e))]
                      [(equal? tag (element-name e)) e]
                      [else #f])))))))

;; The srcs an element needs, so they can be copied next to the program.
(define (element-media e)
  (define acc '())
  (define (note! v) (when (string? v) (set! acc (cons v acc))))
  (let walk ([e e])
    (cond
      [(picture? e) (note! (picture-src e))
                    (when (image-fill? (picture-fill e))
                      (note! (image-fill-src (picture-fill e))))]
      [(shape? e) (when (image-fill? (shape-fill e))
                    (note! (image-fill-src (shape-fill e))))]
      [(group? e) (for-each walk (group-children e))]
      [else (void)]))
  (remove-duplicates acc))

;; Slides added in the editor, written into the program as `def slide_N`
;; definitions and entered in `all_slides`.
;;
;; The definitions go after the last existing one, whatever order the slides
;; belong in: `all_slides` is what decides the deck's order, so appending keeps
;; the edit to two splices instead of renumbering and reflowing the file.
(define (apply-added-slides! program-path actions slide-sites layout d
                            media-names media-subdir)
  (define text (file->string program-path))
  (define taken
    (for/list ([ss (in-list slide-sites)]) (symbol->string (slide-site-scope ss))))
  (define next
    (add1 (apply max 0
                 (for/list ([n (in-list taken)])
                   (cond
                     [(regexp-match #rx"([0-9]+)$" n) => (lambda (m) (string->number (cadr m)))]
                     [else 0])))))
  ;; In deck order, so the names read in the order the slides appear.
  (define (position a)
    ;; (after . seq): where in the program's order, then the order among slides
    ;; added at that same place.
    (define d (sync-action-detail a))
    (+ (* 1000 (first d)) (second d)))
  (define sorted (sort actions < #:key position))
  (define planned
    (for/list ([a (in-list sorted)] [i (in-naturals)])
      (define s (for/first ([s (in-list (deck-slides d))]
                            #:when (= (sync-action-slide a) (slide-index s)))
                  s))
      (list a s (format "slide_~a" (+ next i)))))
  (define missing (filter (lambda (p) (not (second p))) planned))
  (define usable (filter second planned))
  (cond
    [(null? slide-sites)
     (for/list ([p (in-list planned)])
       (cons (first p) "there is no `def slide_N = slide_canvas(...)` to add one beside"))]
    [(not (program-layout-slide-list layout))
     (for/list ([p (in-list planned)])
       (cons (first p) "`all_slides` is not a literal list, so a slide cannot be added to it"))]
    [else
     ;; The images a pasted slide brings with it.
     (for* ([p (in-list usable)]
            [src (in-list (append* (map element-media
                                        (append (slide-inherited (second p))
                                                (slide-elements (second p))))))])
       (copy-media-file! d src program-path media-names media-subdir))
     (define defs
       (string-join
        (for/list ([p (in-list usable)])
          (rhombus-slide-source (second p) (third p)
                                #:media-names media-names
                                #:font (and d (dominant-font d))))
        "\n\n"))
     (define at (slide-site-def-end (last slide-sites)))
     (list (list (rng at at) (string-append "\n\n" defs))
           ;; Each name goes where its slide sits in the deck's order.
           (name-list-edits (program-layout-slide-list layout) usable text)
           (export-edits (program-layout-exports layout) usable)
           missing)]))

;; One edit per position in `all_slides`, since several slides can be added at
;; the same place and two insertions at one offset would fight.
(define (name-list-edits nl planned text)
  (define items (name-list-items nl))
  (define groups
    (let loop ([ps planned] [acc '()])
      (cond
        [(null? ps) (reverse acc)]
        [else
         (define after (first (sync-action-detail (first (car ps)))))
         (define-values (same rest)
           (splitf-at ps (lambda (p) (= after (first (sync-action-detail (first p)))))))
         (loop rest (cons (cons after (map third same)) acc))])))
  (for/list ([g (in-list groups)])
    (define after (car g))
    (define names (cdr g))
    (cond
      ;; After nothing: at the head of the list.
      [(or (zero? after) (null? items))
       (list (rng (add1 (name-list-open nl)) (add1 (name-list-open nl)))
             (string-append (string-join names ", ")
                            (if (null? items) "" ", ")))]
      [else
       (define i (min (sub1 after) (sub1 (length items))))
       (define r (name-entry-range (list-ref items i)))
       (list (rng (rng-end r) (rng-end r))
             (string-append ", " (string-join names ", ")))])))

;; A name nobody exports still works -- `all_slides` is what an export reads --
;; but the generated file lists them all, so a new one is listed too.
(define (export-edits ex planned)
  (cond
    [(not ex) '()]
    [else
     (define at (car ex))
     (define ind (cdr ex))
     (list (list (rng at at)
                 (apply string-append
                        (for/list ([p (in-list planned)])
                          (format "\n~a~a" (make-string ind #\space) (third p))))))]))

(define (copy-media-file! d src program-path media-names media-subdir)
  (define from (build-path (deck-media-dir d) src))
  (define to (build-path (or (path-only (path->complete-path program-path))
                             (current-directory))
                         media-subdir (hash-ref media-names src src)))
  (when (file-exists? from)
    (make-directory* (path-only to))
    (copy-file from to #t)))

(define (apply-actions! program-path actions #:deck [d #f])
  (define-values (all-sites scopes slide-sites layout) (find-program-sites program-path))
  ;; The source text, for the sites that describe a value rather than carry it:
  ;; whether a `~rotate:` says zero, whether a `~flip_h:` says true.
  (define source-text (file->string program-path))
  (check-site-tags all-sites scopes program-path)
  ;; Keyed by the slide's definition, so "Title 1" on slide 3 finds slide 3's
  ;; `at`. When the slide list is computed there is no definition to key on, and
  ;; `check-site-tags` has already established that tags are unique file-wide.
  (define by-scope (for/hash ([s (in-list all-sites)])
                     (values (cons (at-site-scope s) (at-site-tag s)) s)))
  (define by-tag
    (let ([dups (map car (duplicate-tags (map at-site-tag all-sites)))])
      (for/hash ([s (in-list all-sites)] #:unless (member (at-site-tag s) dups))
        (values (at-site-tag s) s))))
  (define (site-for a)
    (define scope (and scopes
                       (<= 1 (sync-action-slide a) (length scopes))
                       (list-ref scopes (sub1 (sync-action-slide a)))))
    (if scope
        (hash-ref by-scope (cons scope (sync-action-tag a)) #f)
        (hash-ref by-tag (sync-action-tag a) #f)))
  ;; Where a new element goes, for the slide an action names.
  (define (slide-site-for a)
    (define scope (and scopes
                       (<= 1 (sync-action-slide a) (length scopes))
                       (list-ref scopes (sub1 (sync-action-slide a)))))
    (and scope (for/first ([ss (in-list slide-sites)]
                           #:when (equal? scope (slide-site-scope ss)))
                 ss)))
  ;; An added image needs its file beside the program, under the same names a
  ;; fresh emit would have used.
  (define media-names (if d (media-names-for d) (hash)))
  (define media-subdir "media")
  (define media? (regexp-match? #rx"media[-_]lookup" (file->string program-path)))
  (define edits '())
  (define applied '())
  (define skipped '())
  ;; An edit with no range writes nothing, so it must not be counted as applied:
  ;; a merge that reports success and changes nothing is the one failure the user
  ;; cannot see. Every caller checks the result.
  (define (edit! r text)
    (and r (begin (set! edits (cons (list r text) edits)) #t)))
  ;; Added slides are done together: several can land at one position, and two
  ;; insertions at one offset would fight.
  (define added-slides
    (filter (lambda (a) (eq? 'added-slide (sync-action-kind a))) actions))
  (unless (null? added-slides)
    (define r (apply-added-slides! program-path added-slides slide-sites layout d
                                   media-names media-subdir))
    (cond
      ;; A list of (action . reason) means none of them could be written.
      [(andmap pair? r)
       (set! skipped (append (reverse r) skipped))]
      [else
       (define-values (edit-lists refused) (values (take r 3) (fourth r)))
       (for ([e (in-list (append* (map (lambda (x) (if (and (pair? x) (rng? (car x)))
                                                       (list x) x))
                                       edit-lists)))])
         (set! edits (cons e edits)))
       (for ([p (in-list refused)])
         (set! skipped (cons (cons (first p) "the deck has no such slide") skipped)))
       (for ([a (in-list added-slides)]
             #:unless (memq a (map first refused)))
         (set! applied (cons a applied)))]))
  (for ([a (in-list actions)] #:unless (eq? 'added-slide (sync-action-kind a)))
    (define site (site-for a))
    (current-source-text source-text)
    ;; An action either lands or it does not. One that writes part of itself and
    ;; then finds it cannot write the rest is reported as refused, and what it
    ;; had written is dropped: a half-applied edit is how a program stops
    ;; compiling.
    (define before-edits edits)
    (case (sync-action-kind a)
      [(moved resized)
       (define g (sync-action-detail a))
       (define-values (x y w h rot fh fv) (apply values g))
       (cond
         [(not site) (set! skipped (cons (cons a "no tagged `at` form in the source") skipped))]
         ;; A computed position has no number to rewrite, so the drag is
         ;; recorded as a correction on `at` instead. Because it is one
         ;; argument rather than a wrapper, a second drag updates these two
         ;; numbers -- corrections cannot stack up the way nested pads do.
         [(not (and (at-site-x site) (at-site-y site)))
          (define prior (sync-action-prior a))
          (cond
            [(not prior)
             (set! skipped (cons (cons a "its position is computed and the program has no such element")
                                 skipped))]
            [(not (at-site-insert-at site))
             (set! skipped (cons (cons a "its position is computed and its tag is not a literal")
                                 skipped))]
            [else
             (define existing (at-site-nudge site))
             (define dx (+ (if existing (second existing) 0.0) (- x (first prior))))
             (define dy (+ (if existing (third existing) 0.0) (- y (second prior))))
             (define wrote?
               (cond
                 [existing (edit! (first existing) (nudge->source dx dy))]
                 [else (edit! (rng (at-site-insert-at site) (at-site-insert-at site))
                              (nudge-argument->source dx dy))]))
             ;; The size may still be a literal even when the position is not.
             (when (eq? 'resized (sync-action-kind a))
               (when (and (at-site-width site) (at-site-height site))
                 (edit! (at-site-width site) (num->source w))
                 (edit! (at-site-height site) (num->source h))))
             (if wrote?
                 (set! applied (cons a applied))
                 (set! skipped (cons (cons a "its existing correction has no source extent")
                                     skipped)))])]
         [else
          (edit! (at-site-x site) (num->source x))
          (edit! (at-site-y site) (num->source y))
          ;; A rotation and a mirror are edits like any other, and both used to
          ;; be dropped in silence: a rotate was counted as applied while
          ;; nothing was written, and a flip -- what dragging a line's endpoint
          ;; past the other end does -- was not even noticed.
          (define turned? (turn-changed? site rot))
          (define mirrored? (mirror-changed? site fh fv))
          (cond
            [(eq? 'resized (sync-action-kind a))
             (cond
               [(and (at-site-width site) (at-site-height site))
                (edit! (at-site-width site) (num->source w))
                (edit! (at-site-height site) (num->source h))
                (set! applied (cons a applied))]
               [else
                (set! skipped (cons (cons a "its size is computed, not a literal") skipped))])]
            [(or turned? mirrored?)
             ;; Written when there is something to write to, and said plainly
             ;; when there is not.
             (define wrote-turn? (or (not turned?) (write-turn! site rot edit!)))
             (define wrote-mirror? (or (not mirrored?) (write-mirror! site fh fv edit!)))
             (if (and wrote-turn? wrote-mirror?)
                 (set! applied (cons a applied))
                 (set! skipped
                       (cons (cons a (cond
                                       [(not wrote-turn?) "its rotation cannot be written here"]
                                       [else "its mirroring cannot be written here"]))
                             skipped)))]
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
      ;; A shape added in the editor is written into the slide it was added to,
      ;; last, which is where the editor put it in the z-order.
      [(added)
       (define ss (slide-site-for a))
       (define e (added-element d (sync-action-slide a) (sync-action-tag a)))
       (define srcs (if e (element-media e) '()))
       (cond
         [(not ss)
          (set! skipped (cons (cons a "no `slide-canvas` call to add it to") skipped))]
         [(not e)
          (set! skipped (cons (cons a "it is not a shape this can write as source") skipped))]
         [(and (pair? srcs) (not media?))
          ;; The image would need a `media` lookup the program does not have,
          ;; and adding one is a restructuring, not a literal edit.
          (set! skipped (cons (cons a "it is an image and the program has no media directory")
                              skipped))]
         [else
          (for ([src (in-list srcs)])
            (define from (build-path (deck-media-dir d) src))
            (define to (build-path (or (path-only (path->complete-path program-path))
                                       (current-directory))
                                   media-subdir (hash-ref media-names src src)))
            (when (file-exists? from)
              (make-directory* (path-only to))
              (copy-file from to #t)))
          ;; Duplicating a shape in the editor gives two of them one name, and
          ;; two `at` forms under one tag is a program a sync cannot read -- so
          ;; a name already spoken for in this slide gets a fresh one.
          (define taken
            (for/list ([st (in-list all-sites)]
                       #:when (equal? (slide-site-scope ss) (at-site-scope st)))
              (at-site-tag st)))
          (define named
            (let loop ([n 2] [name (element-name e)])
              (cond
                [(not (member name taken)) (element-with-name e name)]
                [(> n 99) (element-with-name e name)]
                [else (loop (add1 n) (format "~a (~a)" (element-name e) n))])))
          (define src-text
            (rhombus-element-source
             named (slide-site-indent ss)
             #:media-names media-names
             #:font (and d (dominant-font d))))
          (edit! (rng (slide-site-insert-at ss) (slide-site-insert-at ss))
                 (string-append ",\n" src-text))
          (set! applied (cons a applied))])]
      ;; Deleted in the editor: the `at` form goes, and nothing else.
      [(removed)
       (define whole (and site (at-site-whole site)))
       (cond
         [(not site) (set! skipped (cons (cons a "no tagged `at` form in the source") skipped))]
         [(not whole)
          (set! skipped (cons (cons a "its `at` form has no source extent") skipped))]
         [else (edit! (deletion-range (file->string program-path) whole) "")
               (set! applied (cons a applied))])]
      ;; Appearance: written where the source states it as a literal, and
      ;; reported by name where it does not.
      [(restyle)
       (define sites (if site (at-site-styles site) '()))
       (define (site-for property)
         (findf (lambda (st) (eq? property (style-site-property st))) sites))
       (define detail (sync-action-detail a))
       ;; A fill or an outline the shape did not have, or one the editor took
       ;; away, is a whole argument: everything inside it is written or removed
       ;; at once, because none of those properties has anywhere of its own to
       ;; sit.
       (define (whole-argument head keyword)
         (define ch (assq head detail))
         (define hit (and ch (site-for head)))
         (define (took? ok) (and ok (hash-ref STYLE-GROUPS head)))
         ;; Only a plain colour has a literal inside the argument that means the
         ;; whole of it. Anything else -- nothing at all, a gradient -- changes
         ;; the argument itself.
         (define (colour? v) (and (string? v) (not (equal? "gradient" v))))
         (and hit ch
              (not (and (colour? (second ch)) (colour? (third ch))))
              (cond
                [(eq? ABSENT (third ch))
                 (and (style-site-whole hit)
                      (took? (edit! (style-site-whole hit) "#false")))]
                [(argument->source head detail)
                 => (lambda (src)
                      (took?
                       (cond
                         [(and (style-site-whole hit) (not (style-site-range hit)))
                          (edit! (style-site-whole hit) src)]
                         [(and (style-site-insert-at hit)
                               (eq? keyword (style-site-keyword hit)))
                          (define at (style-site-insert-at hit))
                          (edit! (rng at at)
                                 (format "~a~~~a: ~a"
                                         (argument-comma (current-source-text) at)
                                         (keyword->string keyword) src))]
                         [else #f])))]
                [else #f])))
       (define wholes (append (or (whole-argument 'fill '#:fill) '())
                              (or (whole-argument 'line '#:line) '())))
       (define changes
         (filter (lambda (ch) (not (memq (first ch) wholes))) detail))
       (define-values (done left)
         (for/fold ([done (filter (lambda (p) (memq p '(fill line))) wholes)]
                    [left '()])
                   ([ch (in-list changes)])
           (define property (first ch))
           (define want (third ch))
           (define hit (site-for property))
           (cond
             ;; Gone. Only a property with a whole argument of its own can be
             ;; written as absent -- a fill can, a typeface cannot.
             [(eq? ABSENT want)
              (cond
                [(and hit (style-site-whole hit) (edit! (style-site-whole hit) "#false"))
                 (values (cons property done) left)]
                [else (values done (cons (format "~a was removed, and the program has no way to say that"
                                                 property)
                                         left))])]
             ;; "gradient" is all the comparison knows of one, and it is not
             ;; something to write anywhere: not over the colour that was
             ;; there, and certainly not into a shared definition.
             [(not (statable? property want))
              (values done (cons (format "the ~a was made a gradient, which the merge does not write back"
                                         property)
                                 left))]
             [(and hit (style-site-range hit)
                   (edit! (style-site-range hit) (style->source property want)))
              (values (cons property done) left)]
             ;; Not stated, so it is added -- which is the difference between
             ;; "make this line dashed" working and being reported.
             [(and hit (not (style-site-range hit)) (not (style-site-shared hit))
                   (style-site-insert-at hit) (style-site-keyword hit)
                   (let ([at (style-site-insert-at hit)])
                     (edit! (rng at at)
                            (format "~a~~~a: ~a"
                                    (argument-comma (current-source-text) at)
                                    (keyword->string (style-site-keyword hit))
                                    (added->source property want)))))
              (values (cons property done) left)]
             [(and hit (style-site-shared hit))
              ;; A named colour belongs to everything that uses it, so it is
              ;; rewritten only when everything that uses it changed the same
              ;; way -- the same rule as a tag that names several elements.
              (define name (style-site-shared hit))
              (define users (sites-using all-sites property name))
              (define agreed?
                (and (hash-ref (program-layout-globals layout) name #f)
                     (for/and ([st (in-list users)])
                       (for/or ([b (in-list actions)])
                         (and (eq? 'restyle (sync-action-kind b))
                              (equal? (at-site-tag st) (sync-action-tag b))
                              (for/or ([c (in-list (sync-action-detail b))])
                                (and (eq? (first c) property)
                                     (equal? (third c) want))))))))
              (cond
                [agreed?
                 (edit! (hash-ref (program-layout-globals layout) name)
                        (style->source property want))
                 (values (cons (format "~a via ~a" property name) done) left)]
                [else
                 (values done (cons (format "~a is ~a, shared with ~a other element~a that did not change with it"
                                            property name (max 0 (sub1 (length users)))
                                            (if (= 2 (length users)) "" "s"))
                                    left))])]
             [else (values done (cons (format "~a is not a literal here" property) left))])))
       (cond
         [(null? left) (set! applied (cons a applied))]
         [else
          (set! skipped
                (cons (cons a (string-append
                               (if (null? done) "" (format "~a written; " (reverse done)))
                               (string-join (reverse left) "; ")))
                      skipped))])]
      ;; The order the slides are in, which `all_slides` states.
      [(reordered)
       (define nl (program-layout-slide-list layout))
       (define items (and nl (name-list-items nl)))
       (define order (sync-action-detail a))
       (cond
         [(or (not items) (not (= (length items) (length order))))
          (set! skipped (cons (cons a "`all_slides` is not a literal list of every slide")
                              skipped))]
         [else
          ;; Rewritten whole, because the order is the list rather than any one
          ;; entry in it.
          (define names (for/list ([i (in-list order)])
                          (symbol->string (name-entry-name (list-ref items (sub1 i))))))
          (edit! (rng (add1 (name-list-open nl)) (name-list-close nl))
                 (string-join names ", "))
          (set! applied (cons a applied))])]
      [(ambiguous restacked)
       (set! skipped (cons (cons a (sync-action-detail a)) skipped))]
      [else (set! skipped (cons (cons a "reported only") skipped))])
    (unless (memq a applied) (set! edits before-edits)))
  (when (pair? edits) (splice-file! program-path edits))
  (values (reverse applied) (reverse skipped)))

;; Whether the deck's rotation or mirroring differs from what the source says.
;; The source's own value is what it was exported with, so the base is not
;; needed: a `~rotate:` that is not there means zero, and a flip that is not
;; there means false.
(define (turn-changed? site rot)
  (define r (at-site-rot site))
  (define was (if r (string->number (substring (current-source-text)
                                               (rng-start r) (rng-end r)))
                  0.0))
  (> (abs (- (or was 0.0) rot)) 0.01))

(define (flag-value site get)
  (define r (get site))
  (and r (regexp-match? #rx"true" (substring (current-source-text)
                                             (rng-start r) (rng-end r)))))

(define (mirror-changed? site fh fv)
  (or (not (eq? (flag-value site at-site-flip-h) (and fh #t)))
      (not (eq? (flag-value site at-site-flip-v) (and fv #t)))))

(define (write-turn! site rot edit!)
  (cond
    [(at-site-rot site) (edit! (at-site-rot site) (num->source rot))]
    [(at-site-insert-at site)
     (edit! (rng (at-site-insert-at site) (at-site-insert-at site))
            (format ", ~~rotate: ~a" (num->source rot)))]
    [else #f]))

(define (write-mirror! site fh fv edit!)
  (define (one range want name)
    (cond
      [range (edit! range (if want "#true" "#false"))]
      [(not want) #t]                       ; nothing there and none wanted
      [(at-site-leaf-at site)
       (edit! (rng (at-site-leaf-at site) (at-site-leaf-at site))
              (format "~~~a: #true, " name))]
      [else #f]))
  (and (one (at-site-flip-h site) (and fh #t) "flip_h")
       (one (at-site-flip-v site) (and fv #t) "flip_v")))

;; Every `at` in the program whose `property` is the shared name `name`.
(define (sites-using sites property name)
  (for/list ([st (in-list sites)]
             #:when (for/or ([sy (in-list (at-site-styles st))])
                      (and (eq? property (style-site-property sy))
                           (eq? name (style-site-shared sy)))))
    st))

;; A style value as it reads in source: a colour is the string inside `hex`, a
;; size is a number, a typeface a string, boldness a boolean.
;; Properties that live inside one argument, so they are written and removed
;; together: a fill's opacity is part of its colour, and a line's width is part
;; of its stroke. The first of each is the one that says whether the argument is
;; there at all.
(define STYLE-GROUPS
  (hash 'fill '(fill fill-opacity)
        'line '(line line-width dash)))

;; A whole argument, for a group the source does not state at all: an outline a
;; shape was given in the editor is a stroke call, and a fill is a colour.
(define (argument->source head changes)
  (define (val p)
    (let ([ch (assq p changes)]) (and ch (not (eq? ABSENT (third ch))) (third ch))))
  (case head
    [(fill)
     (define c (val 'fill))
     (define o (val 'fill-opacity))
     ;; "gradient" is all the comparison knows of one: which stops and which
     ;; angle is not something either side can say, so it is reported instead.
     (cond
       [(not (string? c)) #f]
       [(equal? "gradient" c) #f]
       [(and o (< o 0.999)) (format "hex(~s, ~~alpha: ~a)" c (num->source o))]
       [else (format "hex(~s)" c)])]
    [else
     (format "make_stroke(hex(~s)~a~a)"
             (or (val 'line) "000000")
             (let ([w (val 'line-width)]) (if w (format ", ~~width: ~a" (num->source w)) ""))
             (let ([d (val 'dash)])
               (if (and d (not (equal? "solid" (format "~a" d))))
                   (format ", ~~dash: ~a" (style->source 'dash d))
                   "")))]))

;; An argument being added rather than rewritten: a colour is a call of its own,
;; where the literal inside an existing one is just the string.
(define (added->source property value)
  (case property
    [(text-color) (format "hex(~s)" value)]
    [else (style->source property value)]))

;; Whether the source can be given this value at all. A fill the editor made a
;; gradient cannot: which stops and which angle is not something either side of
;; the comparison can say.
(define (statable? property value)
  (case property
    [(fill line text-color) (not (equal? "gradient" value))]
    [else #t]))

(define (style->source property value)
  (case property
    [(fill line text-color) (format "~s" value)]
    [(size line-width fill-opacity) (num->source value)]
    [(font) (format "~s" value)]
    [(bold italic) (if value "#true" "#false")]
    [(space-before space-after) (num->source value)]
    [(wrap) (if value "#true" "#false")]
    [(insets) (format "insets(~a)" (string-join (map num->source value) ", "))]
    ;; `(percent . 1.5)` and `(points . 18.0)` -- the runtime takes either.
    [(line-spacing) (format "pair(#'~a, ~a)" (car value) (num->source (cdr value)))]
    ;; A hyphen is subtraction in Rhombus, so a name that is not an identifier
    ;; there has to be written the long way.
    [(dash align anchor autofit) (let ([n (format "~a" value)])
              (if (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*$" n)
                  (format "#'~a" n)
                  (format "#'#{~a}" n)))]
    [else (format "~a" value)]))

;; The correction's value, as a Rhombus list.
(define (nudge->source dx dy)
  (format "[~a, ~a]" (num->source dx) (num->source dy)))

;; A whole `~nudge:` argument, inserted just after the tag.
(define (nudge-argument->source dx dy)
  (format ", ~~nudge: ~a" (nudge->source dx dy)))

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

;; The state both sides last agreed on. It is derived -- deleting it means the
;; next pass just records where things stand -- so it lives in scratch with the
;; deck rather than beside the program, which holds the program and its images
;; and nothing else.
(define (base-path-for program-path)
  (define full (path->complete-path program-path))
  (define dir (or (path-only full) (current-directory)))
  (build-path dir ".glide"
              (path->string (path-replace-extension (file-name-from-path full)
                                                    ".sync.rktd"))))

;; One merge pass: read both sides, merge against the base, patch the source,
;; and record the new agreed state.
(define (sync-once program-path pptx-path
                   #:workdir [workdir #f]
                   #:dry-run? [dry-run? #f])
  (define base-file (base-path-for program-path))
  (define-values (base _p _d) (read-sync-base base-file))
  (define prog (program-slide-states program-path))
  ;; The deck is unzipped to be read. When the caller did not say where, this
  ;; owns the scratch and clears it before returning.
  (define given-dir workdir)
  (define dir (or workdir (make-temporary-file "syncdeck~a" 'directory)))
  (define deck-ir (pptx->deck pptx-path #:workdir dir))
  (define deck (deck->slide-states deck-ir))
  (define (done! v)
    (when (and (not given-dir) (not (current-keep-work?)))
      (delete-directory/files dir #:must-exist? #f))
    v)
  (cond
    ;; With no base there is nothing to merge against: the program is the truth
    ;; and this pass just records where both sides stand.
    [(not base)
     (unless dry-run?
       (write-sync-base base-file prog
                        #:program (path->string (path->complete-path program-path))
                        #:deck (path->string (path->complete-path pptx-path))))
     (done! (sync-report '() '() '() (not dry-run?)))]
    [else
     (define actions (merge-states base prog deck program-path))
     (cond
       [dry-run? (done! (sync-report actions '() '() #f))]
       [else
        (define-values (applied skipped)
          (apply-actions! program-path actions #:deck deck-ir))
        ;; The new base is the program as it now reads, so the next pass
        ;; compares against something both sides agree on.
        (define after (program-slide-states program-path))
        (write-sync-base base-file after
                         #:program (path->string (path->complete-path program-path))
                         #:deck (path->string (path->complete-path pptx-path)))
        (done! (sync-report actions applied skipped #t))])]))

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
     ;; Grouped by reason. A deck can have thirty shapes the merge refuses for
     ;; one structural reason, and thirty identical lines bury the edits that
     ;; did apply.
     (define by-reason
       (let loop ([sks (sync-report-skipped r)] [order '()] [h (hash)])
         (cond
           [(null? sks) (for/list ([why (in-list (reverse order))])
                          (cons why (reverse (hash-ref h why))))]
           [else
            (define why (cdr (car sks)))
            (loop (cdr sks)
                  (if (hash-has-key? h why) order (cons why order))
                  (hash-update h why (lambda (v) (cons (sync-action-tag (car (car sks))) v))
                               '()))])))
     (for ([g (in-list by-reason)])
       (define tags (cdr g))
       (cond
         [(= 1 (length tags))
          (fprintf o "    not applied: ~s -- ~a\n" (first tags) (car g))]
         [else
          (fprintf o "    not applied, ~a of them -- ~a\n" (length tags) (car g))
          (fprintf o "      ~a~a\n"
                   (string-join (map (lambda (t) (format "~s" t)) (take tags (min 4 (length tags))))
                                ", ")
                   (if (> (length tags) 4)
                       (format " and ~a more" (- (length tags) 4))
                       ""))]))
     (fprintf o "  ~a applied, ~a reported\n"
              (length (sync-report-applied r))
              (- (length as) (length (sync-report-applied r))))])
  (get-output-string o))
