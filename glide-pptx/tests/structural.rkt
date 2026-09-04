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
         glide-pptx/export glide-pptx/sync-state glide-pptx/sync
         "ir-diff.rkt")

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

  (define bs (deck->slide-states before #:include-inherited? #t #:descend-groups? #t))
  (define as (deck->slide-states after #:include-inherited? #t #:descend-groups? #t))

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

;; ------------------------------------------------- a picture used as a fill

;; A shape whose fill is a picture exported as no fill at all: nothing in the
;; drawing description held the file, so the picture was dropped on the way
;; out -- and since the deck is rewritten from the program on every save, the
;; first one lost it. The program renders it, so this only ever showed up in
;; the file.
(define-runtime-path media-dir "media")

(let ()
  (define dir (build-path work "image-fill"))
  (make-directory* (build-path dir "media"))
  (copy-file (build-path media-dir "checker.png")
             (build-path dir "media" "checker.png") #t)
  (define program (build-path dir "f.rhm"))
  (define deck (build-path dir "f.pptx"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export:"
          "  all_slides"
          "def media = media_lookup(\"media\")"
          "def slide_1 = slide_canvas("
          "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
          "  at(60.0, 60.0, ~tag: \"Filled\","
          "     shape_pict(~width: 200.0, ~height: 150.0,"
          "                ~fill: image_fill(media(\"checker.png\"), 0.5)))"
          ")"
          "def all_slides = [slide_1]"
          "")
    "\n")
   program #:exists 'replace)
  (picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
  (define d (pptx->deck deck #:workdir (build-path dir "w")))
  (define e (first (slide-elements (first (deck-slides d)))))
  (check-true (image-fill? (shape-fill e)) "the fill came back as a picture")
  (check-= (image-fill-opacity (shape-fill e)) 0.5 0.01 "with its opacity")
  ;; The importer names a part rather than a path, so the file it points at is
  ;; the one the package holds.
  (check-regexp-match #rx"^ppt/media/" (format "~a" (image-fill-src (shape-fill e))))
  (check-true (file-exists? (build-path dir "w" (image-fill-src (shape-fill e))))
              "and the file it names is in the package"))

(printf "structural tests done; ~a difference~a in total\n"
        total-diffs (if (= 1 total-diffs) "" "s"))
