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
         (only-in "emit-rhombus.rkt" rhombus-element-source))
(provide (struct-out sync-action) (struct-out sync-report)
         program-slide-states deck-slide-states load-program-picts
         match-elements merge-states
         apply-actions! sync-once
         find-at-sites find-program-sites
         (struct-out at-site) (struct-out slide-site) (struct-out rng)
         base-path-for format-sync-report current-keep-work?)

;; Whether scratch directories are left behind for inspection.
(define current-keep-work? (make-parameter #f))

;; kind: 'moved, 'resized, 'retext, 'conflict, 'added, 'removed, 'ambiguous
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
     (error 'glide-pptx
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
    (cond
      ;; Text is the code's, so a program edit wins and a deck-only edit is
      ;; taken.
      [(and deck-retext? prog-retext?)
       (sync-action 'conflict tag index
                    (list 'text (el-state-text b) (el-state-text p) (el-state-text d))
                    (and p (el-geometry p)))]
      [deck-retext? (sync-action 'retext tag index (el-state-text d) #f)]
      [else #f]))))

;; The merge pairs slides by index, so it can only run when the deck holds the
;; same slides the base does. Adding or removing slides in the editor -- pasting
;; some in from another deck, say -- shifts every later index, and then the merge
;; would compare slide 3 against slide 2 and take the difference for edits: on a
;; three-slide deck with one slide pasted at the front, that was 28 "edits" and
;; eight elements deleted from the program. So it refuses instead.
;;
;; The program is the source of truth for what slides exist. Add a slide by
;; adding a `def slide_N` and putting it in `all_slides`; the next export puts it
;; in the deck.
(define (check-slide-sets base deck program-path)
  (define nb (length base))
  (define nd (length deck))
  (unless (= nb nd)
    (error 'glide-pptx
           (string-append
            "the deck now has ~a slide~a and ~a has ~a, so edits cannot be matched up.\n"
            "  Slides added or deleted in the editor are not merged back: the program\n"
            "  says which slides exist. Add a `def slide_N`, put it in `all_slides`,\n"
            "  and export again.\n"
            "  To start over from the deck as it now stands, delete ~a and translate it.")
           nd (if (= 1 nd) "" "s")
           (if (path? program-path) (file-name-from-path program-path) program-path)
           nb
           (file-name-from-path (base-path-for program-path)))))

;; Even with the counts equal the slides can have been swapped around -- one
;; pasted in and one deleted. A slide whose elements mostly do not match is not
;; the slide the base recorded, and calling the difference an edit would delete
;; the program's real elements.
(define WHOLESALE 0.5)

(define (wholesale-change? base-elements actions)
  (define n (length base-elements))
  (define gone (for/sum ([a (in-list actions)]
                         #:when (memq (sync-action-kind a) '(removed))) 1))
  (and (> n 1) (> gone (* WHOLESALE n))))

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
                                '(slide-missing) #f))]))))

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
(struct at-site (tag x y rot width height texts nudge insert-at scope whole) #:transparent)

;; Where a new element goes in one slide's definition: just after the last
;; argument of its `slide-canvas` call, at that argument's indentation. Adding a
;; shape in the editor puts it on top, which is where the last argument draws.
(struct slide-site (scope insert-at indent) #:transparent)
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
    (error 'glide-pptx
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
    (error 'glide-pptx
           (string-append "~a is not Rhombus source, so edits cannot be merged into it.\n"
                          "  Rendering and export work on any program; syncing needs a .rhm.")
           s))
  (rhombus-at-sites path))

(define (find-at-sites path)
  (define-values (sites _scopes _slides) (find-program-sites path))
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
  (values (reverse sites) (rhombus-slide-scopes groups)
          (parameterize ([current-range-offset after-lang])
            (with-indents (rhombus-slide-sites groups text) (reverse sites) text))))

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
                        [ins (and r (canvas-insertion text (rng-end r) rhombus-close))])
                   (and ins (slide-site scope (first ins) 2)))))))

;; `(group def slide_1 (op =) ...)` -> slide_1, and the same for `fun`.
(define (rhombus-def-name g)
  (define l (let ([e (syntax-e g)]) (and (list? e) e)))
  (and l (>= (length l) 3) (eq? 'group (syntax-e* (first l)))
       (memq (syntax-e* (second l)) '(def fun))
       (symbol? (syntax-e* (third l)))
       (syntax-e* (third l))))

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
                  #f #f))))

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

(define (apply-actions! program-path actions #:deck [d #f])
  (define-values (all-sites scopes slide-sites) (find-program-sites program-path))
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
  (for ([a (in-list actions)])
    (define site (site-for a))
    (case (sync-action-kind a)
      [(moved resized)
       (define g (sync-action-detail a))
       (define-values (x y w h rot) (apply values g))
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
          (define src-text
            (rhombus-element-source
             e (slide-site-indent ss)
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
      [(ambiguous)
       (set! skipped (cons (cons a (sync-action-detail a)) skipped))]
      [else (set! skipped (cons (cons a "reported only") skipped))]))
  (when (pair? edits) (splice-file! program-path edits))
  (values (reverse applied) (reverse skipped)))

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
     (check-slide-sets base deck program-path)
     (define actions (merge-states base prog deck))
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
     (for ([sk (in-list (sync-report-skipped r))])
       (fprintf o "    not applied: ~s -- ~a\n" (sync-action-tag (car sk)) (cdr sk)))
     (fprintf o "  ~a applied, ~a reported\n"
              (length (sync-report-applied r))
              (- (length as) (length (sync-report-applied r))))])
  (get-output-string o))
