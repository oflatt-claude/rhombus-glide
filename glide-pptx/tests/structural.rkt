#lang racket/base
;; Import, export, import again, and compare the two intermediate
;; representations directly.
;;
;; Fidelity is otherwise measured in pixels, where a dropped attribute hides
;; easily: a colour that reverts to black on a small shape, a rotation lost on
;; something square, a run's boldness. This comparison is exact and says which
;; field of which element changed, which is the difference between "0.3%, fine"
;; and knowing what was lost.
(require rackunit racket/list racket/string racket/file racket/path racket/format
         racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/render glide-pptx/runtime
         glide-pptx/export glide-pptx/sync-state)

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-structural"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; EMU is a 12700th of a point, so that is the floor on agreement.
(define EPS 0.01)

(define decks
  (sort (for/list ([f (in-list (directory-list decks-dir))]
                   #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
          (path->string (path-replace-extension f "")))
        string<?))

;; Every way one element can differ from another, named.
(define (element-diffs a b)
  (filter
   values
   (list
    (and (not (eq? (el-state-kind a) (el-state-kind b)))
         (format "kind ~a -> ~a" (el-state-kind a) (el-state-kind b)))
    (and (not (el-geometry-same? a b))
         (format "geometry ~a -> ~a"
                 (map (lambda (v) (~r v #:precision 2)) (el-geometry a))
                 (map (lambda (v) (~r v #:precision 2)) (el-geometry b))))
    (and (not (string=? (el-state-text a) (el-state-text b)))
         (format "text ~s -> ~s" (el-state-text a) (el-state-text b)))
    (and (not (string=? (el-state-paint a) (el-state-paint b)))
         (format "paint ~s -> ~s" (el-state-paint a) (el-state-paint b))))))

(define total-diffs 0)

(for ([name (in-list decks)])
  (define dir (build-path work name))
  (make-directory* dir)
  (define original (build-path decks-dir (string-append name ".pptx")))

  (define before (pptx->deck original #:workdir (build-path dir "a")))
  (define exported (build-path dir "round.pptx"))
  (picts->pptx (deck->picts before) exported
               #:width (deck-width before) #:height (deck-height before))
  (define after (pptx->deck exported #:workdir (build-path dir "b")))

  (check-= (deck-width after) (deck-width before) 0.01 (format "~a: slide width" name))
  (check-= (deck-height after) (deck-height before) 0.01 (format "~a: slide height" name))
  (check-equal? (length (deck-slides after)) (length (deck-slides before))
                (format "~a: slide count" name))

  (define bs (deck->slide-states before #:include-inherited? #t))
  (define as (deck->slide-states after #:include-inherited? #t))

  (define reported '())
  (for ([b (in-list bs)] [a (in-list as)])
    (define by-tag (for/hash ([e (in-list (slide-state-elements a))]
                              #:when (el-state-tag e))
                     (values (el-state-tag e) e)))
    (for ([e (in-list (slide-state-elements b))])
      (define tag (el-state-tag e))
      (define hit (and tag (hash-ref by-tag tag #f)))
      (cond
        [(not tag) (void)]   ; nothing to match on; the pixel tests cover it
        [(not hit)
         (set! reported (cons (format "slide ~a: ~s vanished"
                                      (slide-state-index b) tag)
                              reported))]
        [else
         (for ([d (in-list (element-diffs e hit))])
           (set! reported (cons (format "slide ~a: ~s ~a" (slide-state-index b) tag d)
                                reported)))])))
  (set! total-diffs (+ total-diffs (length reported)))
  (printf "~a: ~a element~a, ~a difference~a\n"
          (~a name #:min-width 20)
          (for/sum ([s (in-list bs)]) (length (slide-state-elements s)))
          (if (= 1 (for/sum ([s (in-list bs)]) (length (slide-state-elements s)))) "" "s")
          (length reported) (if (= 1 (length reported)) "" "s"))
  (for ([r (in-list (reverse reported))]) (printf "    ~a\n" r))
  (check-equal? reported '()
                (format "~a: the representation survived the round trip" name)))

(printf "structural tests done; ~a difference~a in total\n"
        total-diffs (if (= 1 total-diffs) "" "s"))
