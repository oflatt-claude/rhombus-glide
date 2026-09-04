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
         (only-in "runtime.rkt" current-media-base current-default-font)
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
;; 'repainted, 'conflict, 'added, 'removed, 'grouped, 'added-slide,
;; 'removed-slide, 'ambiguous
;; `prior` is where the program currently draws the element, which is what a
;; correction for a computed position is measured against. It is #f when the
;; program has no element by that name.
(struct sync-action (kind tag slide detail prior) #:transparent)
;; `skipped` pairs each action the merge could not make with the reason. Knowing
;; an edit was refused, and why, is the whole difference between "nothing moved"
;; and "your drag was thrown away".
;; `notes` are differences the merge saw and did not act on, because they are
;; not assertions: an editor that states nothing leaves the format's defaults
;; standing, and those are not edits. They belong in the report and must not
;; stop a save, which is why they are neither applied nor skipped.
(struct sync-report (actions applied skipped notes base-written?) #:transparent)

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
    (define pg (pict->page p w h))
    (items->slide-state i w h (display-page-items pg)
                        #:background (display-page-background pg))))

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

;; The properties an editor can take away, which are exactly the ones the
;; program can state the absence of: a shape with no fill says `~fill: #false`,
;; a line with no arrowhead `~head: #false`, a paragraph with no bullet
;; `no_bullet`.
;;
;; Nothing else has an "absent" for anyone to choose. There is no command in
;; any editor that removes a typeface, an anchor or a line spacing -- they only
;; ever change. So a side that does not state one is a side that did not say,
;; not a removal, and the two sides differ in what they bother to state:
;; Keynote's export leaves out a great deal that our own writes. Reading that
;; as edits reported hundreds of removals nobody had made, and under the rule
;; that a save lands whole or not at all, one of them blocked the lot.
;; The list is `STYLE-GROUPS`' own members -- everything that travels with a
;; fill or an outline, since an outline being added is its width and its dash
;; arriving with it -- plus the two that state their own absence.
;; A bullet is not on the list: a paragraph with none says `<a:buNone/>`, which
;; is a statement and compares as one, while a paragraph that says nothing at
;; all about bullets inherits whatever its list style gives it.
(define REMOVABLE '(fill fill-opacity line line-width dash cap head tail crop))

;; What the format means when it says nothing. A shape whose `<a:rPr>` states no
;; weight is not bold; one whose `<a:pPr>` states no alignment is left-aligned.
;; Our own export states all of it; other editors stay quiet and let the
;; defaults stand, and reading that as an edit rewrote a program's typography
;; to the defaults -- Georgia 24 bold became Calibri 18 plain, silently, on the
;; first save after a round trip through Keynote.
;;
;; So a deck value that is the default is not taken as an assertion about the
;; shape. It is noted instead: the program keeps what it says, and the report
;; says the two do not agree. The cost is that setting a property *to* its
;; default in the editor is not merged -- which is the one case that cannot be
;; told from the editor saying nothing at all.
;; The typeface a deck falls back on, which is the one the program itself names
;; -- a generated program says `current_default_font(...)` at the top, and the
;; deck we wrote from it says the same in its master. Read off the runtime once
;; the program has been loaded.
(define inherited-font (make-parameter #f))

(define INHERITED-DEFAULTS
  (hash 'size 18.0 'bold #f 'italic #f 'underline #f 'strike #f
        'spacing 0.0 'caps 'none 'baseline 0.0
        'align 'left 'line-spacing '(percent . 1.0)
        'space-before 0.0 'space-after 0.0
        'level 0 'margin-left 0.0 'indent 0.0
        'anchor 'top 'wrap #t 'autofit 'none
        'insets '(7.2 3.6 7.2 3.6)))

;; (values edits notes): the changes to act on, and the ones only worth saying.
(define (split-trusted changes)
  (for/fold ([edits '()] [notes '()] #:result (values (reverse edits) (reverse notes)))
            ([ch (in-list changes)])
    (define d (case (property-head (first ch))
                [(font) (or (inherited-font) 'no-default)]
                [else (hash-ref INHERITED-DEFAULTS (property-head (first ch)) 'no-default)]))
    (if (and (not (eq? 'no-default d)) (equal? d (third ch)))
        (values edits (cons ch notes))
        (values (cons ch edits) notes))))

;; What the two states disagree about, as (property was now). An outline the
;; editor added, or took away, is as much of a change as one it recoloured, so
;; a removable property missing from either side is reported with `ABSENT`.
(define (style-changes was now)
  (define (value l k) (let ([p (assoc k l)]) (if p (cdr p) ABSENT)))
  (define keys (remove-duplicates (append (map car was) (map car now))))
  (filter values
          (for/list ([k (in-list keys)])
            (define a (value was k))
            (define b (value now k))
            (and (not (equal? a b))
                 (or (not (or (eq? ABSENT a) (eq? ABSENT b)))
                     (memq (property-head k) REMOVABLE))
                 (list k a b)))))

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
;; The order the deck draws a slide's shapes in, when that is not the order the
;; program does. One action for the slide, listing the tags in the new order.
(define (restacking index pairs added removed prog-by-tag)
  ;; The tags the program knows them by, which is the base side of each pair:
  ;; a copy made in the editor carries the tag it was copied from, and the
  ;; program has already given it one of its own.
  (define tags (map (lambda (p) (el-state-tag (cdr p))) pairs))
  ;; Read in the order the deck draws them, the places the program draws them.
  (define in-deck-order
    (sort pairs < #:key (lambda (p) (el-state-z (car p)))))
  (define places (map (lambda (p) (el-state-z (cdr p))) in-deck-order))
  (cond
    ;; A shape that came or went is a different slide to reason about, and a tag
    ;; naming several elements has no order of its own to give.
    [(or (pair? added) (pair? removed)) '()]
    [(not (andmap values tags)) '()]
    [(check-duplicates tags) '()]
    [(for/or ([t (in-list tags)]) (> (length (hash-ref prog-by-tag t '())) 1)) '()]
    ;; Already in that order, so nothing was restacked.
    [(equal? places (sort places <)) '()]
    [else
     (list (sync-action 'restacked (format "slide ~a" index) index
                        (map (lambda (p) (el-state-tag (cdr p))) in-deck-order)
                        #f))]))

;; What each group on one deck slide holds, as group tag -> child tags. The
;; merge's own view of a slide has a group as one element, which is what the
;; editor drags; grouping and ungrouping are the two edits that need to see
;; inside one.
(define (deck-group-children d index)
  (define s (and d (for/first ([s (in-list (deck-slides d))]
                               #:when (= index (slide-index s)))
                     s)))
  (for/fold ([h (hash)]) ([e (in-list (if s (slide-elements s) '()))])
    (cond
      [(and (group? e) (not (string=? "" (element-name e))))
       (hash-set h (element-name e)
                 (let leaves ([es (group-children e)])
                   (append*
                    (for/list ([c (in-list es)])
                      (cond
                        [(group? c) (leaves (group-children c))]
                        [(string=? "" (element-name c)) '()]
                        [else (list (element-name c))])))))]
      [else h])))

;; The tag of the element the deck draws just under this one, out of those the
;; program already has: what a new `at` form should follow.
(define (drawn-under d pairs)
  (define under (for/list ([p (in-list pairs)]
                           #:when (and (el-state-tag (cdr p))
                                       (< (el-state-z (car p)) (el-state-z d))))
                  p))
  (and (pair? under)
       (el-state-tag (cdr (argmax (lambda (p) (el-state-z (car p))) under)))))

(define (merge-slide index base prog deck size #:group-children [group-children (hash)])
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
  ;; The elements the deck no longer has, by tag, and the groups that turn out
  ;; to hold exactly them.
  (define removed-by-tag
    (for/hash ([b (in-list deck-removed)] #:when (el-state-tag b))
      (values (el-state-tag b) b)))
  (define groupings
    (for/list ([d (in-list deck-added)]
               #:when (let ([kids (and (el-state-tag d)
                                       (hash-ref group-children (el-state-tag d) #f))])
                        (and (pair? kids)
                             (for/and ([k (in-list kids)]) (hash-ref removed-by-tag k #f)))))
      d))
  (define into-a-group
    (for*/hash ([d (in-list groupings)]
                [k (in-list (hash-ref group-children (el-state-tag d)))])
      (values k #t)))
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
   ;; Bringing a shape to the front changes the order everything is drawn in
   ;; and nothing else. That is the order of the `at` forms, so it is one edit
   ;; on the slide rather than one per shape -- and it only makes sense when
   ;; every shape on the slide is accounted for and answers to its own tag.
   (restacking index deck-pairs deck-added deck-removed prog-by-tag)
   ;; Grouping is one edit, not an addition and two deletions: a group the deck
   ;; has that the base does not, holding exactly elements the base has that
   ;; the deck does not. Read as three separate edits it looks like most of the
   ;; slide being thrown away, which is a slide the merge refuses to touch.
   (for/list ([d (in-list groupings)])
     (sync-action 'grouped (el-state-tag d) index
                  (list (el-geometry d)
                        (for/list ([t (in-list (hash-ref group-children (el-state-tag d)))])
                          (cons t (el-geometry (hash-ref removed-by-tag t)))))
                  #f))
   ;; A shape drawn in the editor sits somewhere in the drawing order, and
   ;; that order is the order of the `at` forms -- so the new form says which
   ;; form it goes after. Writing it last instead put every new shape on top,
   ;; and the deck is rewritten from the program before the order could be
   ;; merged separately.
   (for/list ([d (in-list deck-added)] #:unless (memq d groupings))
     (sync-action 'added (or (el-state-tag d) "(unnamed)") index
                  (list (el-geometry d) (drawn-under d deck-pairs)) #f))
   (for/list ([b (in-list deck-removed)] #:unless (hash-ref into-a-group (el-state-tag b) #f))
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
    ;; What the editor changed about the look of it. Appearance is the code's, so
    ;; this is reported and written only where the source holds a literal -- but
    ;; reported it must be: recolouring a shape in the editor used to disappear
    ;; without a word.
    (let-values ([(changes _noted)
                  (split-trusted (style-changes (el-state-style b) (el-state-style d)))])
      (and (pair? changes) (sync-action 'restyle tag index changes #f)))
    ;; And what it says the shape looks like without ever having said so: the
    ;; defaults it left standing where it stated nothing. Those are noted, not
    ;; acted on, and they do not stop a save.
    (let-values ([(_changes noted)
                  (split-trusted (style-changes (el-state-style b) (el-state-style d)))])
      (and (pair? noted) (sync-action 'noted tag index noted #f)))
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
    ;; Everything on a slide can be deleted, or a slide can be filled from
    ;; empty. There are no tags to go on then, and the one thing left is where
    ;; the slide sits -- which is enough, since the alternative reads as a
    ;; slide deleted and another one added.
    [(or (null? ta) (null? tb))
     (if (= (slide-state-index a) (slide-state-index b)) 0.75 0.0)]
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
  ;; Whatever is left over pairs up by where it sits, when a slide of the same
  ;; number is left over on the other side too. Tag overlap cannot follow every
  ;; edit -- grouping two shapes of three leaves a slide looking like a
  ;; different one -- and reading that as a slide deleted and another added is
  ;; the worse of the two guesses. `wholesale-change?` is what catches a slide
  ;; that really was swapped.
  (for* ([d (in-list deck)]
         #:unless (hash-ref used-d d #f)
         [b (in-list base)]
         #:unless (hash-ref used-b b #f)
         #:when (= (slide-state-index d) (slide-state-index b)))
    (hash-set! used-d d b)
    (hash-set! used-b b #t))
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
  ;; A shape that went into a group is still on the slide -- the group is what
  ;; the editor drags now. Counting it as gone read grouping as a slide swapped
  ;; for a different one.
  (define gone (for/sum ([a (in-list actions)]
                         #:when (memq (sync-action-kind a) '(removed))) 1))
  (and (> n 1) (> gone (* WHOLESALE n))))

;; A slide deleted in the editor is not merged back yet, and taking the
;; difference for edits would delete the program's real elements.
;; Deleting a slide in the editor means deleting its definition and its entry in
;; `all_slides`. The definition can be anything, so it is only removed when the
;; program does not otherwise mention its name: anything else is a program the
;; merge would be rewriting rather than following.
(define (slides-removed removed)
  (for/list ([s (in-list removed)])
    (sync-action 'removed-slide (format "slide ~a" (slide-state-index s))
                 (slide-state-index s) (list (slide-state-index s)) #f)))

(define (merge-states base prog deck [program-path "the program"] #:deck-ir [deck-ir #f])
  (define-values (pairs added removed) (match-slides base deck))
  ;; A slide the base has that the deck does not, *and* a slide the deck has
  ;; that the base does not, in the same merge: that is a slide the matching
  ;; could not follow, not a deletion and a new slide. Telling those apart is a
  ;; guess, and guessing wrong deletes a definition the program still wants --
  ;; grouping two shapes of three is enough to make a slide stop matching.
  (define muddled? (and (pair? added) (pair? removed)))
  ;; A deck has one size, so resizing it in the editor is one edit however many
  ;; slides there are -- and the program usually says it once, as a name every
  ;; canvas shares.
  (define resized
    (let* ([p (and (pair? pairs) (first pairs))]
           [d (and p (car p))] [b (and p (cdr p))])
      (if (and p (or (> (abs (- (slide-state-width d) (slide-state-width b))) 0.05)
                     (> (abs (- (slide-state-height d) (slide-state-height b))) 0.05)))
          (list (sync-action 'resized-deck "the deck" (slide-state-index b)
                             (list (slide-state-width d) (slide-state-height d)) #f))
          '())))
  (append
   resized
   (if muddled?
       (for/list ([b (in-list removed)])
         (sync-action 'ambiguous (format "slide ~a" (slide-state-index b))
                      (slide-state-index b)
                      (format (string-append "this slide and ~a in the deck no longer look like"
                                             " each other, so which is which is a guess;"
                                             " nothing here is rewritten")
                              (if (= 1 (length added)) "one" "some"))
                      #f))
       '())
   (if muddled? '() (slides-removed removed))
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
                                 (max 1.0 (slide-state-width bs))
                                 #:group-children
                                 (deck-group-children deck-ir (slide-state-index ds))))
         (define bg
           (let ([was (slide-state-background bs)] [now (slide-state-background ds)])
             (if (equal? was now)
                 '()
                 ;; The program's own paint decides a conflict the same way an
                 ;; element's does: if the code changed it too, the code wins
                 ;; and the editor's is reported.
                 (list (sync-action
                        (if (equal? was (slide-state-background ps)) 'repainted 'conflict)
                        (format "slide ~a" index) index
                        (list (list 'background (or was ABSENT) (or now ABSENT))) #f)))))
         (if (wholesale-change? (slide-state-elements bs) as)
             (list (sync-action
                    'ambiguous (format "slide ~a" index) index
                    (format (string-append
                             "most of this slide's ~a elements are not in the deck any more,"
                             " so it is a different slide rather than an edited one")
                            (length (slide-state-elements bs)))
                    #f))
             (append as bg))]
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
   (if muddled? '()
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
                        acc)))])))))

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
(struct slide-site (scope insert-at indent def-start def-end background width height)
  #:transparent)
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
                   (and ins shut
                        (slide-site scope (first ins) 2
                                    (let ([d (range-of (second l))]) (and d (rng-start d)))
                                    (add1 shut)
                                    (canvas-paint-site (sixth l))
                                    (canvas-size-site (sixth l) '#:width 'slide-width)
                                    (canvas-size-site (sixth l) '#:height 'slide-height))))))))

;; Where the canvas states its background, which is always somewhere: the
;; emitter writes one whether the slide had a background of its own or not.
;; What the canvas says its size is: a number to rewrite, or a name that every
;; slide shares -- a generated program says `~width: slide_width`, so the deck
;; being resized is one edit on one `def`.
(define (canvas-size-site parens kw property)
  (define stx (parens-kw-group parens kw))
  (define literal (and stx (literal-range (single-term stx) real?)))
  (define name (and stx (not literal) (single-symbol stx)))
  (and stx (style-site property literal name #f kw #f)))

;; The one term a group holds, when it holds one.
(define (single-term g)
  (define l (and (syntax? g) (let ([e (syntax-e g)]) (and (list? e) e))))
  (and l (= 2 (length l)) (second l)))

(define (single-symbol g)
  (define t (single-term g))
  (and t (symbol? (syntax-e* t)) (syntax-e* t)))

(define (canvas-paint-site parens)
  (define stx (parens-kw-group parens '#:background))
  (define hit (and stx (not (compound-fill? stx)) (hex-site stx)))
  (and stx
       (style-site 'background
                   (and hit (eq? 'literal (car hit)) (cdr hit))
                   (and hit (eq? 'shared (car hit)) (cdr hit))
                   #f '#:background
                   (value-extent-of stx))))

;; `(group def slide_1 (op =) ...)` -> slide_1, and the same for `fun`.
(define (rhombus-def-name g)
  (define l (let ([e (syntax-e g)]) (and (list? e) e)))
  (and l (>= (length l) 3) (eq? 'group (syntax-e* (first l)))
       (memq (syntax-e* (second l)) '(def fun))
       (symbol? (syntax-e* (third l)))
       (syntax-e* (third l))))

;; `def brand = hex("4472C4")` -> brand -> the range of that string, and
;; `def slide_width = 720.0` -> slide_width -> the range of that number. A value
;; given a name is shared, so changing one shape that uses it is not the same
;; edit as changing the name.
(define (rhombus-global-colours groups)
  (for/fold ([h (hash)]) ([g (in-list groups)])
    (define l (let ([e (syntax-e g)]) (and (list? e) e)))
    (define nm (and l (>= (length l) 5) (eq? 'def (syntax-e* (second l)))
                    (symbol? (syntax-e* (third l)))
                    (syntax-e* (third l))))
    (define hit (and nm (hex-site g)))
    (define number (and nm (not hit) (= 5 (length l)) (literal-range (fifth l) real?)))
    (cond
      [(and hit (eq? 'literal (car hit))) (hash-set h nm (cdr hit))]
      [number (hash-set h nm number)]
      [else h])))

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
                  (rhombus-child-paragraph-texts child)
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
;; A `[...]` value's extent. The bracket wrapper carries no position, but its
;; head term spans the brackets and everything between them.
(define (bracket-extent g)
  (define l (and (syntax? g) (let ([e (syntax-e g)]) (and (list? e) e))))
  (define br (and l (findf (lambda (x) (rhombus-head? x 'brackets)) l)))
  (and br (range-of (first (syntax-e br)))))

(define (quoted-range g)
  (define l (and (syntax? g) (let ([e (syntax-e g)]) (and (list? e) e))))
  (define terms (if (and (pair? l) (eq? 'group (syntax-e* (first l)))) (cdr l) (or l '())))
  (and (= 2 (length terms))
       (let ([a (range-of (first terms))] [b (range-of (second terms))])
         (and a b (rng (rng-start a) (rng-end b))))))

;; A keyword's value inside an argument list already in hand.
(define (parens-kw-group parens kw)
  (and parens
       (for/or ([g (in-list (cdr (syntax-e parens)))])
         (and (eq? kw (rhombus-kw-name g)) (rhombus-kw-group g)))))

;; The extent of a whole value, when it is a call.
(define (value-extent-of stx)
  (define n (call-name stx))
  (and n (rhombus-call-extent (current-source-text) n)))

;; The first string a named call is given, wherever it appears: `media("x.png")`
;; is a call like `hex("...")`, and its argument is what says which file.
(define (call-string-range stx name)
  (define found (box #f))
  (let walk ([s stx])
    (define l (and (syntax? s) (let ([e (syntax-e s)]) (and (list? e) e))))
    (when (and l (not (unbox found)))
      (for ([a (in-list l)] [b (in-list (cdr l))])
        (when (and (not (unbox found)) (eq? name (syntax-e* a)) (rhombus-head? b 'parens))
          (define args (cdr (syntax-e b)))
          (define v (and (pair? args) (rhombus-group-value (first args))))
          (when (and v (string? (syntax-e* v))) (set-box! found (range-of v)))))
      (unless (unbox found) (for-each walk l))))
  (unbox found))

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
                             [(list) (bracket-extent g)]
                             [(flag) (literal-range (kw-single-stx call kw) boolean?)]
                             [else (literal-range (kw-single-stx call kw) how)])
                           #f #f kw #f)
               (style-site property #f #f (call-append-at call) kw #f)))))
  ;; Which file a picture draws, which the source names inside `media(...)`.
  (define (media-site call)
    (and call
         (let ([r (call-string-range call 'media)])
           (and r (style-site 'image r #f #f #f #f)))))
  ;; How much of a picture is cropped away: a list when there is a crop and
  ;; nothing at all when there is not, so it is added and removed like a fill.
  (define (crop-site call)
    (define g (and call (kw-value-stx call '#:crop)))
    (define r (and g (or (bracket-extent g)
                         (literal-range (kw-single-stx call '#:crop) boolean?))))
    (cond
      [(and g r) (style-site 'crop r #f #f '#:crop r)]
      [g #f]
      [call (style-site 'crop #f #f (call-append-at call) '#:crop #f)]
      [else #f]))
  ;; An arrowhead is a `line_end(...)` call when there is one and `#false` when
  ;; there is not, so both the value and its absence are written in place.
  (define (end-site property call kw)
    (and call
         (let* ([g (kw-value-stx call kw)]
                [r (and g (or (value-extent g) (literal-range (kw-single-stx call kw) boolean?)))])
           (cond
             [(and g r) (style-site property r #f #f kw r)]
             [g #f]
             [else (style-site property #f #f (call-append-at call) kw #f)]))))
  ;; A leaf can be any call at all -- a program refactored into `vstack` and
  ;; `beside` still draws the shape the editor is dragging. So an argument is
  ;; only ever added to a call known to take it: adding `~crop:` to a `vstack`
  ;; is a program that no longer runs.
  (define leaf-name (let ([n (call-name child)]) (and n (syntax-e* n))))
  (define (leaf-taking . names) (and leaf-name (memq leaf-name names) child))
  ;; The colour argument, however the source states it: a `hex(...)` to rewrite,
  ;; a shared name, `#false` for a shape that has none, or nothing at all -- in
  ;; which case the whole argument is added, where there is a call to add it to.
  (define (paint-site property kw stx [addable #f])
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
      [(not stx) (and addable
                      (style-site property #f #f (call-append-at addable) kw #f))]
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
  ;; Every run and every paragraph, numbered as the state numbers them: the
  ;; k-th `run(...)` call in the source is the k-th run of the body, because
  ;; both read them paragraph by paragraph. Bolding one word of a line is a
  ;; change to the run that word is in.
  (define runs (rhombus-run-calls child))
  (define paras (rhombus-para-calls child))
  (define run (and (pair? runs) (first runs)))
  (define one-para (and (pair? paras) (first paras)))
  ;; A run's colour is a call too.
  (define (run-colour-site property r)
    (and r
         (let* ([v (kw-value-stx r '#:color)]
                [hit (and v (hex-site v))])
           (cond
             [hit (style-site property
                              (and (eq? 'literal (car hit)) (cdr hit))
                              (and (eq? 'shared (car hit)) (cdr hit))
                              #f #f #f)]
             ;; Text whose colour the source never states, recoloured in the
             ;; editor: the argument is added to the run.
             [(not v) (style-site property #f #f (call-append-at r) '#:color #f)]
             [else #f]))))
  ;; The runs and the paragraphs, each under its own number.
  (define run-sites
    (append*
     (for/list ([r (in-list runs)] [i (in-naturals 1)])
       (list (kw-site (nth-property 'size i) r '#:size real?)
             (kw-site (nth-property 'font i) r '#:font string?)
             (kw-site (nth-property 'bold i) r '#:bold 'flag)
             (kw-site (nth-property 'italic i) r '#:italic 'flag)
             (kw-site (nth-property 'underline i) r '#:underline 'flag)
             (kw-site (nth-property 'strike i) r '#:strike 'flag)
             (kw-site (nth-property 'spacing i) r '#:spacing real?)
             (kw-site (nth-property 'caps i) r '#:caps 'quoted)
             (kw-site (nth-property 'baseline i) r '#:baseline real?)
             (run-colour-site (nth-property 'text-color i) r)))))
  (define para-sites
    (append*
     (for/list ([p (in-list paras)] [i (in-naturals 1)])
       (list (kw-site (nth-property 'align i) p '#:align 'quoted)
             (kw-site (nth-property 'line-spacing i) p '#:line_spacing 'call)
             (kw-site (nth-property 'space-before i) p '#:space_before real?)
             (kw-site (nth-property 'space-after i) p '#:space_after real?)
             (kw-site (nth-property 'level i) p '#:level real?)
             (kw-site (nth-property 'margin-left i) p '#:margin_left real?)
             (kw-site (nth-property 'indent i) p '#:indent real?)
             (kw-site (nth-property 'bullet i) p '#:bullet 'call)))))
  (append
   (filter values run-sites)
   (filter values para-sites)
   (filter values
          (list (paint-site 'fill '#:fill fill-stx (leaf-taking 'shape_pict))
                opacity
                (paint-site 'line '#:line stroke-stx
                            (leaf-taking 'shape_pict 'image_pict))
                (kw-site 'line-width stroke '#:width real?)
                (kw-site 'dash stroke '#:dash 'quoted)
                (kw-site 'cap stroke '#:cap 'quoted)
                (end-site 'head stroke '#:head)
                (end-site 'tail stroke '#:tail)

                ;; A picture's own arguments.
                (media-site (leaf-taking 'image_pict))
                (kw-site 'opacity (leaf-taking 'image_pict) '#:opacity real?)
                (crop-site (leaf-taking 'image_pict))
                (kw-site 'anchor body-call '#:anchor 'quoted)
                (kw-site 'wrap body-call '#:wrap 'flag)
                (kw-site 'autofit body-call '#:autofit 'quoted)
                (kw-site 'insets body-call '#:insets 'call)))))

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

;; The runs' string literals, grouped by the paragraph they are in: the text of
;; a body is its paragraphs joined by newlines, so where the paragraphs are is
;; what says which run a retyped word landed in.
(define (rhombus-child-paragraph-texts child)
  (define paras (rhombus-para-calls child))
  (if (pair? paras)
      (filter pair? (for/list ([p (in-list paras)]) (rhombus-child-texts p)))
      (let ([rs (rhombus-child-texts child)]) (if (null? rs) '() (list rs)))))

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

(define (apply-actions! program-path actions #:deck [d #f] #:atomic? [atomic? #f])
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
  (define (site-for a) (site-for-tag a (sync-action-tag a)))
  ;; The same, for a tag other than the action's own: a grouping names the
  ;; group, and what it moves are the elements inside it.
  (define (site-for-tag a tag)
    (define scope (and scopes
                       (<= 1 (sync-action-slide a) (length scopes))
                       (list-ref scopes (sub1 (sync-action-slide a)))))
    (if scope
        (hash-ref by-scope (cons scope tag) #f)
        (hash-ref by-tag tag #f)))
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
  (define notes '())
  ;; Ungrouping is several actions at once -- the group gone, and each shape it
  ;; held arriving on its own -- so it is worked out before anything is
  ;; written. Lifting the `at` forms out of the `group_pict` keeps what the
  ;; code says about them; deleting the group and writing the shapes from the
  ;; deck would replace a shared colour with a literal and drop every comment.
  (define lifts (ungroupings actions all-sites site-for-tag))
  (define consumed
    (for*/hasheq ([l (in-list lifts)] [a (in-list (cons (first l) (second l)))])
      (values a #t)))
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
  ;; The lifts, before the loop: each replaces one `at` form with the forms it
  ;; held, and the actions it answers are done with.
  (for ([l (in-list lifts)])
    (current-source-text source-text)
    (define gone (first l))
    (define inner (third l))
    (define g (sync-action-detail gone))
    (define ind (indent-at source-text (rng-start (at-site-whole (site-for gone)))))
    (define pieces
      (for/list ([st (in-list inner)])
        (define whole (at-site-whole st))
        (reindent
         (splice-string
          (substring source-text (rng-start whole) (rng-end whole))
          (list (cons (shift-range (at-site-x st) (rng-start whole))
                      (num->source (+ (first g) (at-site-number st at-site-x source-text))))
                (cons (shift-range (at-site-y st) (rng-start whole))
                      (num->source (+ (second g) (at-site-number st at-site-y source-text))))))
         (- ind (indent-at source-text (rng-start whole))))))
    (edit! (at-site-whole (site-for gone))
           (string-join pieces (string-append ",\n" (spaces ind))))
    (for ([a (in-list (cons gone (second l)))])
      (set! applied (cons a applied))))
  (for ([a (in-list actions)] #:unless (or (eq? 'added-slide (sync-action-kind a))
                                           (hash-ref consumed a #f)))
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
       (define paras (and site (at-site-texts site)))
       (define want (sync-action-detail a))
       (define hit (and paras (retyped-run paras want source-text)))
       (cond
         [(not site) (set! skipped (cons (cons a "no tagged `at` form in the source") skipped))]
         [(or (not paras) (null? paras))
          (set! skipped (cons (cons a "its text is not written as literals here") skipped))]
         [(eq? 'crosses hit)
          (set! skipped
                (cons (cons a (string-append "the retyping crosses runs or paragraphs,"
                                             " so which of them it belongs to is a guess"))
                      skipped))]
         [(not hit)
          (set! skipped (cons (cons a "its text is not written as literals here") skipped))]
         [else (edit! (car hit) (format "~s" (cdr hit)))
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
          ;; After the form it is drawn over, or before the first of them when
          ;; it is drawn under everything. A slide whose forms cannot be found
          ;; takes it last, which is where the editor usually put it anyway.
          (define under
            (let ([d (sync-action-detail a)])
              (and (list? d) (= 2 (length d)) (second d))))
          (define after (and under (site-for-tag a under)))
          (define first-form
            (let ([here (for/list ([st (in-list all-sites)]
                                   #:when (and (equal? (slide-site-scope ss) (at-site-scope st))
                                               (at-site-whole st)))
                          st)])
              (and (pair? here)
                   (argmin (lambda (st) (rng-start (at-site-whole st))) here))))
          (cond
            [(and after (at-site-whole after))
             (define at (rng-end (at-site-whole after)))
             (edit! (rng at at) (string-append ",\n" src-text))]
            [(and (not under) first-form)
             (define at (line-start source-text (rng-start (at-site-whole first-form))))
             (edit! (rng at at) (string-append src-text ",\n"))]
            [else
             (edit! (rng (slide-site-insert-at ss) (slide-site-insert-at ss))
                    (string-append ",\n" src-text))])
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
         (findf (lambda (st) (equal? property (style-site-property st))) sites))
       (define detail (sync-action-detail a))
       ;; A fill or an outline the shape did not have, or one the editor took
       ;; away, is a whole argument: everything inside it is written or removed
       ;; at once, because none of those properties has anywhere of its own to
       ;; sit.
       (define (whole-argument head keyword)
         (define ch (assoc head detail))
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
         (filter (lambda (ch) (not (member (first ch) wholes))) detail))
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
                                                 (property-name property))
                                         left))])]
             ;; A picture swapped for another: the file comes to sit beside the
             ;; program and the source is pointed at it. Rewriting the name
             ;; alone would leave it naming a file that is not there.
             [(equal? 'image property)
              (define file (and d (picture-file-for d a)))
              (define hit (site-for 'image))
              (cond
                [(not media?)
                 (values done (cons "the picture was replaced and the program has no media directory"
                                    left))]
                [(or (not file) (not hit) (not (style-site-range hit)))
                 (values done (cons "the picture was replaced and the program does not name its file"
                                    left))]
                [else
                 (define name (copy-media-in! file program-path media-subdir))
                 (cond
                   [(and name (edit! (style-site-range hit) (format "~s" name)))
                    (values (cons 'image done) left)]
                   [else (values done (cons "the picture was replaced and its file could not be copied"
                                            left))])])]
             ;; "gradient" is all the comparison knows of one, and it is not
             ;; something to write anywhere: not over the colour that was
             ;; there, and certainly not into a shared definition.
             [(not (statable? property want))
              (values done (cons (format "the ~a was made a gradient, which the merge does not write back"
                                         (property-name property))
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
                                (and (equal? (first c) property)
                                     (equal? (third c) want))))))))
              (cond
                [agreed?
                 (edit! (hash-ref (program-layout-globals layout) name)
                        (style->source property want))
                 (values (cons (format "~a via ~a" (property-name property) name) done) left)]
                [else
                 (values done (cons (format "~a is ~a, shared with ~a other element~a that did not change with it"
                                            (property-name property) name (max 0 (sub1 (length users)))
                                            (if (= 2 (length users)) "" "s"))
                                    left))])]
             [else (values done (cons (format "~a is not a literal here" (property-name property))
                                      left))])))
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
      ;; A slide deleted in the editor: its definition goes, and its entry in
      ;; `all_slides` with it. Anything else in the program that names it is a
      ;; reason to stop -- the merge follows the program, it does not rewrite it.
      [(removed-slide)
       (define i (sync-action-slide a))
       (define scope (and scopes (<= 1 i (length scopes)) (list-ref scopes (sub1 i))))
       (define ss (slide-site-for a))
       (define nl (program-layout-slide-list layout))
       (define items (and nl (name-list-items nl)))
       (define entry (and items scope
                          (findf (lambda (e) (eq? scope (name-entry-name e))) items)))
       (define def-r (and ss (definition-extent ss source-text)))
       (define entry-r (and entry (entry-extent entry items)))
       (define export-r (and scope (export-entry-extent source-text (symbol->string scope))))
       (define elsewhere
         (and scope (mentions-outside source-text (symbol->string scope)
                                      (list def-r entry-r export-r))))
       (cond
         [(or (not ss) (not entry) (not def-r))
          (set! skipped (cons (cons a "the merge cannot see its definition and its entry in `all_slides`")
                              skipped))]
         [(positive? elsewhere)
          (set! skipped
                (cons (cons a (format (string-append "`~a` is named ~a more time~a in the program,"
                                                     " so deleting the slide is left to you")
                                      scope elsewhere (if (= 1 elsewhere) "" "s")))
                      skipped))]
         [(and (edit! def-r "") (edit! entry-r "")
               (or (not export-r) (edit! export-r "")))
          (set! applied (cons a applied))]
         [else
          (set! skipped (cons (cons a "its definition is not one the merge can remove") skipped))])]
      ;; Two shapes grouped in the editor: their `at` forms move inside a
      ;; `group_pict`, so everything the code says about them survives -- the
      ;; comment above one, a colour shared with something else, a size that is
      ;; computed. Writing the group from the deck instead would throw all of
      ;; that away and rewrite the shapes as literals.
      [(grouped)
       (define g (first (sync-action-detail a)))
       (define kids (second (sync-action-detail a)))
       (define sites (for/list ([k (in-list kids)]) (site-for-tag a (car k))))
       (define clash (site-for-tag a (sync-action-tag a)))
       (cond
         [clash
          (set! skipped (cons (cons a (format "the program already has an `at` tagged ~s"
                                              (sync-action-tag a)))
                              skipped))]
         [(not (andmap values sites))
          (set! skipped (cons (cons a "not all of the shapes it holds are tagged `at` forms here")
                              skipped))]
         [(not (for/and ([st (in-list sites)])
                 (and (at-site-whole st) (at-site-x st) (at-site-y st))))
          (set! skipped (cons (cons a "one of the shapes it holds has a computed position")
                              skipped))]
         [else
          (define text source-text)
          (define pieces
            (for/list ([st (in-list sites)] [k (in-list kids)])
              (define whole (at-site-whole st))
              (define kid (cdr k))
              (splice-string
               (substring text (rng-start whole) (rng-end whole))
               (list (cons (shift-range (at-site-x st) (rng-start whole))
                           (num->source (- (first kid) (first g))))
                     (cons (shift-range (at-site-y st) (rng-start whole))
                           (num->source (- (second kid) (second g))))))))
          ;; The group goes where the first of them was, so it is drawn where
          ;; they were drawn.
          (define anchor (argmin (lambda (st) (rng-start (at-site-whole st))) sites))
          (define ind (indent-at text (rng-start (at-site-whole anchor))))
          (define head (format "at(~a, ~a, ~~tag: ~s,"
                               (num->source (first g)) (num->source (second g))
                               (sync-action-tag a)))
          (define open (format "group_pict(~~width: ~a, ~~height: ~a,"
                               (num->source (third g)) (num->source (fourth g))))
          (define kid-col (+ ind 3 (string-length "group_pict(")))
          (define body
            (for/list ([piece (in-list pieces)] [st (in-list sites)])
              (reindent piece (- kid-col (indent-at text (rng-start (at-site-whole st)))))))
          (edit! (at-site-whole anchor)
                 (string-append
                  head "\n" (spaces (+ ind 3)) open "\n" (spaces kid-col)
                  (string-join body (string-append ",\n" (spaces kid-col)))
                  "))"))
          (for ([st (in-list sites)] #:unless (eq? st anchor))
            (edit! (deletion-range text (at-site-whole st)) ""))
          (set! applied (cons a applied))])]
      ;; The deck's size, which every canvas states -- as a number each, or as
      ;; one name they share.
      [(resized-deck)
       (define want (sync-action-detail a))
       (define targets
         (for*/list ([ss (in-list slide-sites)]
                     [pair (in-list (list (cons (slide-site-width ss) (first want))
                                          (cons (slide-site-height ss) (second want))))]
                     #:when (car pair))
           pair))
       (define resolved
         (for/list ([t (in-list targets)])
           (define site (car t))
           (define value (cdr t))
           (cond
             [(style-site-range site) (cons (style-site-range site) value)]
             [(and (style-site-shared site)
                   (hash-ref (program-layout-globals layout) (style-site-shared site) #f))
              => (lambda (r) (cons r value))]
             [else #f])))
       (cond
         [(null? targets)
          (set! skipped (cons (cons a "no `slide_canvas` states its size here") skipped))]
         [(not (andmap values resolved))
          (set! skipped
                (cons (cons a "a slide states its size as something other than a number or a name")
                      skipped))]
         [else
          ;; One `def` serves every slide, so the same range comes up once per
          ;; slide and is written once.
          (for ([e (in-list (remove-duplicates resolved))])
            (edit! (car e) (num->source (cdr e))))
          (set! applied (cons a applied))])]
      ;; The slide's own paint, which the canvas states.
      [(repainted)
       (define ss (slide-site-for a))
       (define hit (and ss (slide-site-background ss)))
       (define want (third (first (sync-action-detail a))))
       (cond
         [(not hit)
          (set! skipped (cons (cons a "the canvas does not state a background") skipped))]
         [(not (statable? 'background want))
          (set! skipped (cons (cons a (string-append "the background was made a gradient,"
                                                     " which the merge does not write back"))
                              skipped))]
         [(and (style-site-range hit) (string? want)
               (edit! (style-site-range hit) (format "~s" want)))
          (set! applied (cons a applied))]
         [(and (style-site-whole hit) (background->source want)
               (edit! (style-site-whole hit) (background->source want)))
          (set! applied (cons a applied))]
         [else
          (set! skipped (cons (cons a "its background is not a literal here") skipped))])]
      ;; The drawing order, which is the order of the `at` forms.
      [(restacked)
       (define scope (let ([i (sync-action-slide a)])
                       (and scopes (<= 1 i (length scopes)) (list-ref scopes (sub1 i)))))
       (define on-slide (sort (for/list ([st (in-list all-sites)]
                                         #:when (and (equal? scope (at-site-scope st))
                                                     (at-site-whole st)))
                                st)
                              < #:key (lambda (st) (rng-start (at-site-whole st)))))
       ;; An `at` inside a `group_pict` is drawn by its group, not by the
       ;; canvas: only the forms the canvas itself holds have an order of their
       ;; own to change.
       (define here
         (for/list ([st (in-list on-slide)]
                    #:unless (for/or ([o (in-list on-slide)])
                               (and (not (eq? o st))
                                    (encloses? (at-site-whole o) (at-site-whole st)))))
           st))
       (define want (sync-action-detail a))
       (define pieces (reorder-pieces here want source-text))
       (cond
         [(not scope)
          (set! skipped (cons (cons a "the slide it is on is not one the program names") skipped))]
         [(not pieces)
          (set! skipped
                (cons (cons a (string-append "the `at` forms are not all here to reorder"
                                             " -- move them yourself to change the drawing order"))
                      skipped))]
         [(edit! (car pieces) (cdr pieces)) (set! applied (cons a applied))]
         [else (set! skipped (cons (cons a "its `at` forms cannot be moved") skipped))])]
      ;; A difference that is not an assertion: said, and nothing more.
      [(noted)
       (set! notes
             (cons (cons a (string-join
                            (for/list ([ch (in-list (sync-action-detail a))])
                              (format "~a is ~s here and ~s in the deck, which is what a deck says when it says nothing"
                                      (property-name (first ch)) (second ch) (third ch)))
                            "; "))
                   notes))]
      [(ambiguous)
       (set! skipped (cons (cons a (sync-action-detail a)) skipped))]
      [else (set! skipped (cons (cons a "reported only") skipped))])
    (unless (or (memq a applied) (eq? 'noted (sync-action-kind a)))
      (set! edits before-edits)))
  (cond
    ;; All of it, or none of it.
    [(and atomic? (pair? skipped)) (values '() (reverse skipped) (reverse notes))]
    [else
     (when (pair? edits) (splice-file! program-path edits))
     (values (reverse applied) (reverse skipped) (reverse notes))]))

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
                      (and (equal? property (style-site-property sy))
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
        'line '(line line-width dash cap head tail)))

;; A whole argument, for a group the source does not state at all: an outline a
;; shape was given in the editor is a stroke call, and a fill is a colour.
(define (argument->source head changes)
  (define (val p)
    (let ([ch (assoc p changes)]) (and ch (not (eq? ABSENT (third ch))) (third ch))))
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
     (format "make_stroke(hex(~s)~a~a~a~a~a)"
             (or (val 'line) "000000")
             (let ([w (val 'line-width)]) (if w (format ", ~~width: ~a" (num->source w)) ""))
             (let ([d (val 'dash)])
               (if (and d (not (equal? "solid" (format "~a" d))))
                   (format ", ~~dash: ~a" (style->source 'dash d))
                   ""))
             (let ([c (val 'cap)])
               (if (and c (not (eq? 'flat c)))
                   (format ", ~~cap: ~a" (style->source 'cap c))
                   ""))
             (end-argument "head" (val 'head))
             (end-argument "tail" (val 'tail)))]))

;; An arrowhead, where there is one to write.
(define (end-argument name e)
  (if (list? e) (format ", ~~~a: ~a" name (style->source 'head e)) ""))

;; An argument being added rather than rewritten: a colour is a call of its own,
;; where the literal inside an existing one is just the string.
(define (added->source property value)
  (case (property-head property)
    [(text-color) (format "hex(~s)" value)]
    [else (style->source property value)]))

;; Whether the source can be given this value at all. A fill the editor made a
;; gradient cannot: which stops and which angle is not something either side of
;; the comparison can say.
(define (statable? property value)
  (case (property-head property)
    [(fill line text-color background) (not (equal? "gradient" value))]
    [else #t]))

;; A background as one argument: a colour, a colour and its opacity, or none.
;; A definition, the comment heading it, and the newline it ends on. The heading
;; is taken with it because a comment left behind would name the definition
;; after it instead.
(define (definition-extent ss text)
  (define start (slide-site-def-start ss))
  (define end (slide-site-def-end ss))
  (and start end
       (rng (comment-block-start text start)
            (let loop ([j end])
              (cond
                [(>= j (string-length text)) j]
                [(char=? #\newline (string-ref text j)) (add1 j)]
                [(char-whitespace? (string-ref text j)) (loop (add1 j))]
                [else end])))))

;; Back over the comment lines immediately above a position, to the start of the
;; first of them.
(define (comment-block-start text at)
  (let loop ([start (line-start text at)])
    (define prev (and (> start 0) (line-start text (sub1 start))))
    (cond
      [(not prev) start]
      [(regexp-match? #px"^[ \t]*//" (substring text prev start)) (loop prev)]
      [else start])))

(define (line-start text at)
  (let loop ([j (min at (string-length text))])
    (cond [(<= j 0) 0]
          [(char=? #\newline (string-ref text (sub1 j))) j]
          [else (loop (sub1 j))])))

(define (line-end-after text at)
  (let loop ([j (min at (string-length text))])
    (cond [(>= j (string-length text)) j]
          [(char=? #\newline (string-ref text j)) (add1 j)]
          [else (loop (add1 j))])))

;; The line naming a slide in the program's `export:` block, which a generated
;; program has one of per slide. Only inside that block: a bare name on a line
;; of its own means something else anywhere but there.
(define (export-entry-extent text name)
  (define m (regexp-match-positions #px"(?m:^export:[ \t]*$)" text))
  (and m
       (let* ([from (cdar m)]
              [block-end (let loop ([j (line-end-after text (add1 from))])
                           (cond
                             [(>= j (string-length text)) j]
                             [(regexp-match? #px"^[ \t]" (substring text j (min (string-length text) (add1 j))))
                              (loop (line-end-after text j))]
                             [(char=? #\newline (string-ref text j)) (loop (add1 j))]
                             [else j]))]
              [hit (regexp-match-positions (pregexp (format "(?m:^[ \t]+~a[ \t]*\r?\n)"
                                                            (regexp-quote name)))
                                           text from block-end)])
         (and hit (rng (caar hit) (cdar hit))))))

;; How many times a name is written outside the places being removed. A program
;; that names a slide anywhere else is one the merge would be rewriting.
(define (mentions-outside text name ranges)
  (for/sum ([m (in-list (regexp-match-positions*
                         (pregexp (format "(?<![A-Za-z0-9_])~a(?![A-Za-z0-9_])"
                                          (regexp-quote name)))
                         text))]
            #:unless (for/or ([r (in-list ranges)])
                       (and r (<= (rng-start r) (car m)) (< (car m) (rng-end r)))))
    1))

;; One name out of `[a, b, c]`, with the comma that separated it.
(define (entry-extent entry items)
  (define r (name-entry-range entry))
  (define i (index-of items entry))
  (define n (length items))
  (cond
    [(and i (< (add1 i) n)) (rng (rng-start r) (rng-start (name-entry-range (list-ref items (add1 i)))))]
    [(and i (> i 0)) (rng (rng-end (name-entry-range (list-ref items (sub1 i)))) (rng-end r))]
    [else r]))

;; The `at` forms of one slide, written out in a new order. What separates them
;; stays where it is, so the file keeps its indentation and its commas; only the
;; forms themselves move. A comment between two of them is a reason not to: it
;; would end up describing something else.
(define (reorder-pieces sites want text)
  (define by-tag (for/hash ([st (in-list sites)]) (values (at-site-tag st) st)))
  (define ordered (for/list ([t (in-list want)]) (hash-ref by-tag t #f)))
  ;; A comment sitting above an `at` describes that `at`, so it is part of the
  ;; piece that moves. The emitter writes one over every element it adds.
  (define (extent st)
    (rng (comment-block-start text (rng-start (at-site-whole st)))
         (rng-end (at-site-whole st))))
  (define gaps
    (for/list ([a (in-list sites)] [b (in-list (cdr (append sites (list #f))))]
               #:when b)
      (substring text (rng-end (extent a)) (rng-start (extent b)))))
  (and (= (length sites) (length want))
       (andmap values ordered)
       ;; A comment left in a gap belongs to neither side of it.
       (not (for/or ([g (in-list gaps)]) (regexp-match? #rx"//" g)))
       (let* ([span (rng (rng-start (extent (first sites)))
                         (rng-end (extent (last sites))))]
              [texts (for/list ([st (in-list ordered)])
                       (let ([r (extent st)]) (substring text (rng-start r) (rng-end r))))])
         (cons span
               (apply string-append
                      (for/list ([t (in-list texts)] [i (in-naturals)])
                        (string-append t (if (< i (length gaps)) (list-ref gaps i) ""))))))))

;; Which run a retyping landed in, as (range . new-value). A body's text is its
;; runs joined, with a newline between paragraphs, so the run to rewrite is the
;; one the changed stretch falls inside. When it falls across two of them --
;; or across a paragraph break -- there is no answer, only a guess, and
;; 'crosses says so.
;;
;; This is what makes retyping a word of a styled line work: `run("hello ")`
;; followed by a bold `run("world")` has no single literal for the whole line,
;; and one of the two is where the edit belongs.
(define (retyped-run paras want text)
  (define runs
    (append*
     (for/list ([p (in-list paras)] [i (in-naturals)])
       (append (if (zero? i) '() (list (cons #f "\n")))
               (for/list ([r (in-list p)]) (cons r (literal-string r text)))))))
  (define (value-of c) (cdr c))
  (cond
    [(for/or ([c (in-list runs)]) (not (value-of c))) #f]
    [else
     (define was (apply string-append (map value-of runs)))
     ;; The stretch that changed: what is left after the shared ends.
     (define keep-front
       (let loop ([i 0])
         (if (and (< i (string-length was)) (< i (string-length want))
                  (char=? (string-ref was i) (string-ref want i)))
             (loop (add1 i))
             i)))
     (define keep-back
       (let loop ([j 0])
         (if (and (< j (- (string-length was) keep-front))
                  (< j (- (string-length want) keep-front))
                  (char=? (string-ref was (- (string-length was) 1 j))
                          (string-ref want (- (string-length want) 1 j))))
             (loop (add1 j))
             j)))
     (define from keep-front)
     (define to (- (string-length was) keep-back))
     ;; Which run holds [from, to), and where it starts.
     (define-values (found start)
       (for/fold ([found #f] [at 0]) ([c (in-list runs)])
         (define end (+ at (string-length (value-of c))))
         (values (if (and (not found) (<= at from) (<= to end) (car c)) (cons c at) found)
                 end)))
     (void start)
     (cond
       [(not found) 'crosses]
       [else
        (define c (car found))
        (define at (cdr found))
        (define old (value-of c))
        (cons (car c)
              (string-append (substring old 0 (- from at))
                             (substring want from (- (string-length want) keep-back))
                             (substring old (- to at))))])]))

;; A string literal's value, read out of the source it is written in.
(define (literal-string r text)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (define v (read (open-input-string (substring text (rng-start r) (rng-end r)))))
    (and (string? v) v)))

;; A group the deck no longer has, whose `at` form holds exactly the `at` forms
;; the deck now has at top level: that is the editor's Ungroup, and the shapes
;; can be lifted out rather than deleted and written again.
;;
;; Returns (list removed-action added-actions inner-sites) for each.
(define (ungroupings actions all-sites site-of)
  (define (inner-of site)
    (for/list ([st (in-list all-sites)]
               #:when (and (not (eq? st site))
                           (at-site-whole st) (at-site-whole site)
                           (encloses? (at-site-whole site) (at-site-whole st))))
      st))
  (filter
   values
   (for/list ([a (in-list actions)] #:when (eq? 'removed (sync-action-kind a)))
     (define site (site-of a (sync-action-tag a)))
     (define inner (and site (inner-of site)))
     (define tags (and inner (map at-site-tag inner)))
     (define arrived
       (and (pair? (or tags '()))
            (for/list ([b (in-list actions)]
                       #:when (and (eq? 'added (sync-action-kind b))
                                   (= (sync-action-slide a) (sync-action-slide b))
                                   (member (sync-action-tag b) tags)))
              b)))
     (and (pair? (or arrived '()))
          ;; Every shape it held, and nothing else: a group half of which was
          ;; ungrouped is not something to guess at.
          (= (length arrived) (length tags))
          (for/and ([st (in-list inner)])
            (and (at-site-x st) (at-site-y st)))
          (list a arrived inner)))))

;; A number the source states, read back out of it.
(define (at-site-number st get text)
  (define r (get st))
  (or (and r (string->number (string-trim (substring text (rng-start r) (rng-end r))))) 0.0))

;; The file behind a deck's picture, for the element an action names.
(define (picture-file-for d a)
  (define e (added-element d (sync-action-slide a) (sync-action-tag a)))
  (define src (and (picture? e) (picture-src e)))
  (define p (and src (build-path (deck-media-dir d) src)))
  (and p (file-exists? p) p))

;; A file copied to sit beside the program, under a name that is not already
;; taken by different bytes. Returns the name the program should use.
(define (copy-media-in! from program-path subdir)
  (define dir (build-path (or (path-only (path->complete-path program-path))
                              (current-directory))
                          subdir))
  (define base (path->string (file-name-from-path from)))
  (define-values (stem ext)
    (let ([m (regexp-match #px"^(.*?)([.][^.]*)?$" base)])
      (values (second m) (or (third m) ""))))
  (define name
    (let loop ([n 1])
      (define try (if (= n 1) base (format "~a-~a~a" stem n ext)))
      (define to (build-path dir try))
      (cond
        [(not (file-exists? to)) try]
        [(equal? (file-size to) (file-size from)) try]
        [(> n 99) try]
        [else (loop (add1 n))])))
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (make-directory* dir)
    (copy-file from (build-path dir name) #t)
    name))

(define (spaces n) (make-string (max 0 n) #\space))

;; Whether one range holds another, which is how a nested `at` is told from one
;; the canvas holds itself.
(define (encloses? a b)
  (and (<= (rng-start a) (rng-start b)) (<= (rng-end b) (rng-end a))))

;; A range read out of one place, used inside a copy of it.
(define (shift-range r start) (rng (- (rng-start r) start) (- (rng-end r) start)))

;; Edits inside a string, back to front so the offsets hold.
(define (splice-string str edits)
  (for/fold ([s str])
            ([e (in-list (sort edits > #:key (lambda (e) (rng-start (car e)))))])
    (string-append (substring s 0 (rng-start (car e))) (cdr e)
                   (substring s (rng-end (car e))))))

;; A piece of source moved to another column. Its first line goes wherever it
;; is put; the lines under it keep their shape relative to that one.
(define (reindent str delta)
  (define lines (string-split str "\n" #:trim? #f))
  (cond
    [(or (zero? delta) (null? (cdr lines))) str]
    [(positive? delta)
     (string-join lines (string-append "\n" (spaces delta)))]
    [else
     (string-append
      (first lines)
      (apply string-append
             (for/list ([l (in-list (cdr lines))])
               (define drop (min (- delta) (- (string-length l) (string-length (string-trim l #:right? #f)))))
               (string-append "\n" (substring l drop)))))]))

(define (background->source want)
  (cond
    [(eq? ABSENT want) "#false"]
    [(string? want) (format "hex(~s)" want)]
    [(and (list? want) (= 2 (length want)))
     (format "hex(~s, ~~alpha: ~a)" (first want) (num->source (second want)))]
    [else #f]))

;; `font` and `(font 2)` are written the same way; which run they belong to is
;; the site's business, not the value's.
(define (property-head property) (if (pair? property) (first property) property))

;; How a report names one: "font" for the first run, "font of run 2" for the
;; rest, since a body of one run should read the way it always did.
(define (property-name property)
  (if (pair? property)
      (format "~a of ~a ~a" (first property)
              (if (memq (first property)
                        '(align line-spacing space-before space-after
                          level margin-left indent bullet))
                  "paragraph" "run")
              (second property))
      (format "~a" property)))

(define (style->source property0 value)
  (define property (property-head property0))
  (case property
    [(fill line text-color) (format "~s" value)]
    [(size line-width fill-opacity) (num->source value)]
    [(font) (format "~s" value)]
    [(bold italic) (if value "#true" "#false")]
    ;; A level is a whole number of steps, and `1.0` is not the number the
    ;; parser reads back out of `lvl="1"`.
    [(level) (format "~a" (inexact->exact (round value)))]
    [(space-before space-after margin-left indent spacing baseline)
     (num->source value)]
    [(underline strike) (if value "#true" "#false")]
    ;; A bullet is a call of five things, and no bullet at all is `no_bullet`.
    [(bullet)
     (if (list? value)
         (format "bullet(~a, ~a, ~a, ~a, ~a)"
                 (style->source 'dash (first value))
                 (if (second value) (format "~s" (second value)) "#false")
                 (if (third value) (format "~s" (third value)) "#false")
                 (if (fourth value) (num->source (fourth value)) "#false")
                 (if (fifth value) (format "hex(~s)" (fifth value)) "#false"))
         "no_bullet")]
    ;; `line_end(#'triangle, "med", "med")`, or nothing on that end at all.
    [(head tail)
     (if (list? value)
         (format "line_end(~a, ~s, ~s)"
                 (style->source 'dash (first value)) (second value) (third value))
         "#false")]
    [(wrap) (if value "#true" "#false")]
    [(insets) (format "insets(~a)" (string-join (map num->source value) ", "))]
    [(opacity) (num->source value)]
    [(crop) (if (list? value)
                (format "[~a]" (string-join (map num->source value) ", "))
                "#false")]
    ;; `(percent . 1.5)` and `(points . 18.0)` -- the runtime takes either.
    [(line-spacing) (format "pair(#'~a, ~a)" (car value) (num->source (cdr value)))]
    ;; A hyphen is subtraction in Rhombus, so a name that is not an identifier
    ;; there has to be written the long way.
    [(dash align anchor autofit cap caps) (let ([n (format "~a" value)])
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
;; Two paths naming one file, as far as anything here can tell.
(define (same-file-path? a b)
  (define (norm p)
    (with-handlers ([exn:fail? (lambda (_e) (format "~a" p))])
      (path->string (simplify-path (path->complete-path (if (path? p) p (string->path p))) #f))))
  (equal? (norm a) (norm b)))

(define (base-path-for program-path)
  (define full (path->complete-path program-path))
  (define dir (or (path-only full) (current-directory)))
  (build-path dir ".glide"
              (path->string (path-replace-extension (file-name-from-path full)
                                                    ".sync.rktd"))))

;; One merge pass: read both sides, merge against the base, patch the source,
;; and record the new agreed state.
;; `atomic?` is what a save means: either every edit the editor made is written
;; or none of them is. A merge that writes four of five edits and reports the
;; fifth leaves the program and the deck each holding part of the truth, and
;; nobody can say which part -- so the fifth failing takes the other four with
;; it, the program is left exactly as it was, and the deck keeps everything
;; until whatever caused it is resolved.
(define (sync-once program-path pptx-path
                   #:workdir [workdir #f]
                   #:dry-run? [dry-run? #f]
                   #:atomic? [atomic? #f])
  (define base-file (base-path-for program-path))
  (define-values (base recorded-program _d) (read-sync-base base-file))
  ;; The scratch is picked up rather than cleared when a session starts, so
  ;; that edits made while nothing was watching are merged rather than thrown
  ;; away. That only holds while it is *this* program's scratch: a folder
  ;; copied along with a project, or left by another program, holds a deck and
  ;; a base that have nothing to do with this one.
  (when (and base recorded-program
             (not (same-file-path? recorded-program program-path)))
    (error 'glide
           (string-append
            "~a was written for a different program.\n"
            "  it says:  ~a\n"
            "  this is:  ~a\n"
            "  Delete ~a and start again -- it holds a deck and an agreed base\n"
            "  from another session, and merging those into this program would\n"
            "  be merging someone else's edits.")
           (file-name-from-path base-file) recorded-program
           (path->string (path->complete-path program-path))
           (let ([d (path-only base-file)]) (if d (path->string d) base-file))))
  (define prog (program-slide-states program-path))
  ;; The program has been loaded now, so the typeface it falls back on is known.
  (define fallback-font (current-default-font))
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
     (done! (sync-report '() '() '() '() (not dry-run?)))]
    [else
     (define actions
       (parameterize ([inherited-font fallback-font])
         (merge-states base prog deck program-path #:deck-ir deck-ir)))
     (cond
       [dry-run? (done! (sync-report actions '() '() '() #f))]
       [else
        (define-values (applied skipped notes)
          (apply-actions! program-path actions #:deck deck-ir #:atomic? atomic?))
        (cond
          ;; Nothing was written, so there is nothing new for the base to
          ;; record: leaving it alone is what makes the next save try again.
          [(and atomic? (pair? skipped)) (done! (sync-report actions '() skipped notes #f))]
          [else
           ;; The new base is the program as it now reads, so the next pass
           ;; compares against something both sides agree on.
           (define after (program-slide-states program-path))
           (write-sync-base base-file after
                            #:program (path->string (path->complete-path program-path))
                            #:deck (path->string (path->complete-path pptx-path)))
           (done! (sync-report actions applied skipped notes #t))])])]))

(define (format-sync-report r)
  (define o (open-output-string))
  (define as (sync-report-actions r))
  (cond
    [(null? as) (fprintf o "  nothing to merge\n")]
    [else
     (for ([a (in-list as)] #:unless (eq? 'noted (sync-action-kind a)))
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
     ;; Notes, once each. A terse editor can leave the same note on every text
     ;; box on the slide, and they are not things to do -- only things to know.
     (define notes (sync-report-notes r))
     (unless (null? notes)
       (define kinds (remove-duplicates (map cdr notes)))
       (fprintf o "  ~a element~a the deck describes differently, not merged:\n"
                (length notes) (if (= 1 (length notes)) "" "s"))
       (for ([why (in-list (take kinds (min 3 (length kinds))))])
         (fprintf o "    ~a\n" why))
       (when (> (length kinds) 3)
         (fprintf o "    and ~a more like it\n" (- (length kinds) 3))))
     (fprintf o "  ~a applied, ~a reported~a\n"
              (length (sync-report-applied r))
              (- (length as) (length (sync-report-applied r)) (length notes))
              (if (null? notes) "" (format ", ~a noted" (length notes))))])
  (get-output-string o))
