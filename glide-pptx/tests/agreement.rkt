#lang racket/base
;; Does a translated deck agree with the deck it writes? Over the corpus.
;;
;; `structural.rkt` asks this of six fixtures. Five hundred real files ask it of
;; everything the fixtures never thought of, and on the first run it found four
;; things: a table had no state at all, so a deck with a table could not be
;; synced; a picture cropped by nothing disagreed with a picture not cropped, on
;; fifteen decks; a numbered bullet lost its typeface; and a picture whose blip
;; resolved to nothing was written as `media("missing.png")` into a program with
;; no `media` defined, which then did not load.
;;
;; The property is exact. Anything the merge says is a difference between what
;; the program means and what the deck holds, and the next real edit is merged
;; on top of it.
;;
;; The base is recorded by a pass that has none. That pass writes only the base
;; and never touches the program -- and without it every later pass reports
;; nothing whatever the deck holds, which is a test that cannot fail.
;;
;; The decks are not committed -- they belong to LibreOffice and POI. Run
;; `tools/fetch-corpus.sh` to get them; with no corpus present this says so and
;; passes. `GLIDE_AGREE_N` and `GLIDE_AGREE_SKIP` cut it down to a slice while
;; working on one.
(require rackunit/log)
(require rackunit racket/list racket/string racket/file racket/path racket/format
         racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/export glide-pptx/sync
         glide-pptx/emit-rhombus)

(define-runtime-path corpus-dir "corpus")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-agreement"))

(define all
  (if (directory-exists? corpus-dir)
      (sort (for/list ([f (in-list (directory-list corpus-dir))]
                       #:when (regexp-match? #rx"[.]pptx$" (path->string f)))
              (path->string f))
            string<?)
      '()))

(define N (string->number (or (getenv "GLIDE_AGREE_N") "10000")))
(define SKIP (string->number (or (getenv "GLIDE_AGREE_SKIP") "0")))
(define decks (take (drop all (min SKIP (length all)))
                    (min N (max 0 (- (length all) SKIP)))))

;; What still disagrees, named so it stays visible rather than tolerated. Each
;; is a bug; they are listed so the sweep can guard the other five hundred in
;; the meantime.
(define known
  (hash "poi-customGeo.pptx"
        "the second run of a placeholder title comes back white where the program says black"))

(cond
  [(null? decks)
   (printf "no corpus present; run tools/fetch-corpus.sh to fetch one\n")]
  [else
   (current-allow-unsupported? #t)
   (delete-directory/files work #:must-exist? #f)
   (make-directory* work)
   (define agreed 0)
   (define fontless 0)
   (define refused 0)
   (define surprises '())
   (for ([name (in-list decks)] [i (in-naturals 1)])
     (define dir (build-path work (format "d~a" i)))
     (define (note! what) (set! surprises (cons (cons name what) surprises)))
     (with-handlers
         ([(lambda (_e) #t)
           (lambda (e)
             (define msg (first (string-split (exn-message e) "\n")))
             (cond
               ;; The generated program checks its fonts and will not run on
               ;; substitutes, which is its own doing and not a disagreement.
               [(regexp-match? #rx"required font is not installed" msg)
                (set! fontless (add1 fontless))]
               ;; A file we refuse in our own words is a file we refuse. Some of
               ;; these are deliberately corrupt -- POI keeps its fuzzer's
               ;; findings here.
               [(regexp-match? #rx"^glide[-a-z]*:" msg) (set! refused (add1 refused))]
               [else (note! (format "raised: ~a" msg))]))])
       (make-directory* dir)
       (define program (build-path dir "p.rhm"))
       (define pptx (build-path dir "out.pptx"))
       (define d (pptx->deck (build-path corpus-dir name) #:workdir (build-path dir "u")))
       (write-rhombus-deck d program #:source-name name)
       (picts->pptx (load-program-picts program) pptx)
       (define first-pass (sync-once program pptx #:workdir (build-path dir "w")))
       (unless (sync-report-base-written? first-pass)
         (note! "the first pass recorded no base"))
       (define r (sync-once program pptx #:workdir (build-path dir "w") #:dry-run? #t))
       (define as (sync-report-actions r))
       (cond
         [(null? as) (set! agreed (add1 agreed))]
         [else
          (note! (string-join
                  (remove-duplicates
                   (for/list ([a (in-list as)])
                     (format "~a ~s" (sync-action-kind a) (sync-action-tag a))))
                  ", "))])))
   ;; Reported deck by deck, so a new one is named rather than buried in a count.
   (for ([s (in-list (reverse surprises))])
     (cond
       [(hash-ref known (car s) #f)
        => (lambda (why) (printf "  known: ~a -- ~a\n" (car s) why))]
       [else (check-equal? (cdr s) '() (format "~a: nothing to merge" (car s)))]))
   (printf "agreement over ~a decks: ~a agreed, ~a disagreed, ~a refused, ~a needed a font\n"
           (length decks) agreed
           (length (filter (lambda (s) (not (hash-ref known (car s) #f))) surprises))
           refused fontless)])

(module+ main (void (test-log #:display? #t #:exit? #t)))
