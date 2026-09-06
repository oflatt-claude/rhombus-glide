#lang racket/base
;; Showing a converted deck: `staged.rhm`, and the slide that has been given
;; stages by hand.
;;
;; A converted deck is meant to be rewritten, and the first thing anyone does
;; by hand is give a slide stages -- which turns that slide from a canvas into
;; an animated `Pict`. Everything downstream of the canvas has to take both, and
;; the two places that did not were the show, which called `Pict.from_handle` on
;; something that was already a `Pict`, and the backup PDF, which drew the first
;; frame of an animation and called that the slide.
;;
;; The show needs a display; the PDF does not, and is checked either way.
(require rackunit/log)
(require rackunit racket/file racket/path racket/system racket/port racket/string
         racket/runtime-path)

(define-runtime-path here ".")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-staged"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; `exec-file` is however racket was invoked, which is a bare name when it came
;; off the PATH -- and `system*` cannot exec one of those.
(define racket-exe
  (let ([e (find-system-path 'exec-file)])
    (if (absolute-path? e) e (or (find-executable-path e) e))))
;; `xvfb-run` first even where DISPLAY is set: a DISPLAY that names a server
;; nobody is running does not fail, it hangs, and a test that hangs is worse
;; than one that says it could not run.
(define xvfb (find-executable-path "xvfb-run"))
(define display? (or xvfb (getenv "DISPLAY")))

;; Runs a Rhombus program and hands back what it printed.
(define (run-rhm src #:display? [needs-display? #f])
  (define path (build-path work (format "p~a.rhm" (equal-hash-code src))))
  (call-with-output-file path #:exists 'replace (lambda (o) (write-string src o)))
  (define out (open-output-string))
  (define err (open-output-string))
  (define ok?
    (parameterize ([current-output-port out] [current-error-port err])
      (cond
        [(and needs-display? xvfb) (system* xvfb "-a" racket-exe path)]
        [else (system* racket-exe path)])))
  (values ok? (get-output-string out) (get-output-string err)))

(define PROLOGUE
  (string-join
   '("#lang rhombus/and_meta"
     "import:"
     "  pict open"
     "  lib(\"racket/base.rkt\") as rkt"
     "  lib(\"glide-pptx/runtime.rhm\") as glide"
     ""
     "def w = 320.0"
     "def h = 240.0"
     "def canvas = glide.slide_canvas(~width: w, ~height: h)"
     "def panned = glide.slide_canvas(~width: w, ~height: h, ~transition: #'left)"
     "def hidden = glide.slide_canvas(~width: w, ~height: h, ~hidden: #true)"
     "// A slide given stages by hand: three advances, and not a canvas any more."
     "def with_stages:"
     "  def base = Pict.from_handle(canvas)"
     "  switch(base, animate(fun (t): base.alpha(t)), animate(fun (t): base.alpha(t)))"
     "")
   "\n"))

;; One program, one run: a Rhombus module costs more to compile than everything
;; asked of it here, so all of it is asked at once.
(define PROGRAM
  (string-append
   PROLOGUE
   (string-join
    (list
     "import:"
     "  lib(\"slideshow/main.rkt\") as ss"
     "  lib(\"glide-pptx/show.rhm\") open"
     ""
     "println(\"plain \" +& glide.transition_of(canvas))"
     "println(\"panned \" +& glide.transition_of(panned))"
     "println(\"staged \" +& glide.transition_of(with_stages))"
     "println(\"advances \" +& with_stages.duration)"
     ""
     (format "glide.deck_to_pdf([canvas, hidden, with_stages], ~s, ~~width: w, ~~height: h)"
             (path->string (build-path work "deck.pdf")))
     ""
     "// Counted with the reveal and the transitions off, so that what is counted"
     "// is the slides and not the frames they are played through."
     "fun emitted():"
     "  recur count(n = 0):"
     "    if ss.#{most-recent-slide}()"
     "    | block:"
     "        ss.#{retract-most-recent-slide}()"
     "        count(n + 1)"
     "    | n"
     "set_reveal(#false)"
     "set_transitions(#false)"
     "show_slides([canvas, panned, hidden], ~width: w, ~height: h)"
     "println(\"stills \" +& emitted())"
     "show_slides([with_stages], ~width: w, ~height: h)"
     "println(\"stages \" +& emitted())"
     "// And with the reveal on, a still slide is faded up rather than cut to."
     "set_reveal(#true)"
     "show_slides([canvas, panned], ~width: w, ~height: h)"
     "println(\"revealed \" +& emitted())"
     ""
     "// A slide written as a function is built when it is shown and not before,"
     "// which is what makes starting part way through worth anything."
     "def built = Array(0)"
     "fun lazy_slide(n):"
     "  fun ():"
     "    built[0] := built[0] + n"
     "    canvas"
     "set_reveal(#false)"
     "// The slide number counts up across the whole program, so `start_from` is"
     "// set past every slide there could be rather than to a number this test"
     "// would have to know."
     "set_start_from(100000)"
     "show_slides([lazy_slide(1), lazy_slide(10)], ~width: w, ~height: h)"
     "println(\"skipped: built \" +& built[0] +& \", shown \" +& emitted())"
     "set_start_from(0)"
     "show_slides([lazy_slide(100)], ~width: w, ~height: h)"
     "println(\"kept: built \" +& built[0] +& \", shown \" +& emitted())"
     "// A program that has registered slides ends by showing them, and a show"
     "// waits for a keypress that is not coming. Everything asked of it has been"
     "// answered by here, so leave rather than open a window nobody is at."
     "Port.Output.flush()"
     "rkt.#{exit}(0)")
    "\n")
   "\n"))

(cond
  [(not display?)
   (printf "no display and no xvfb-run; the show is not checked\n")]
  [else
   (define-values (ok? out err) (run-rhm PROGRAM #:display? #t))
   (check-true ok? (format "the program ran: ~a" err))

   ;; ------------------------------------------ what the canvas remembers
   (check-regexp-match #rx"plain #false" out "a canvas that named no transition says so")
   (check-regexp-match #rx"panned left" out "and one that named a transition remembers it")
   ;; A slide with stages is no longer the canvas, and nothing pretends otherwise:
   ;; the show falls back to its own default rather than reading through a wrapper.
   (check-regexp-match #rx"staged #false" out "a slide with stages is not a canvas")
   (check-regexp-match #rx"advances 3" out "and it is three advances long")

   ;; ------------------------------------------------------------ the show
   ;; Three slides in, one of them hidden. A slide with stages reaches `slide`
   ;; as a `Pict`, which is what used to raise here.
   (check-regexp-match #rx"stills 2" out "a hidden slide is not shown, and a still slide is one slide")
   (check-regexp-match #px"stages ([2-9]|[0-9][0-9]+)" out
                       "a slide with stages is played out rather than shown once")
   (check-regexp-match #px"revealed ([3-9]|[0-9][0-9]+)" out
                       "and with the reveal on, a still slide is faded up rather than cut to")

   ;; Nothing is built for the slides that are skipped, and the one that is
   ;; shown is built when it is shown. Starting part way through a real talk is
   ;; six of the nine seconds it takes to start.
   (check-regexp-match #rx"skipped: built 0, shown 0" out
                       "a slide the show skips is never built")
   (check-regexp-match #rx"kept: built 100, shown 1" out
                       "and the one it shows is")

   ;; --------------------------------------------------------- the backup PDF
   (define pdf (build-path work "deck.pdf"))
   (check-true (file-exists? pdf) "there is a PDF")
   ;; Counted with `pdfinfo` rather than by reading the file: the page objects
   ;; are in a compressed stream, so there is nothing in the bytes to count.
   (define pdfinfo (find-executable-path "pdfinfo"))
   (cond
     [(not (and pdfinfo (file-exists? pdf)))
      (printf "no pdfinfo; the pages are not counted\n")]
     [else
      (define out (open-output-string))
      (parameterize ([current-output-port out]) (system* pdfinfo (path->string pdf)))
      (define m (regexp-match #px"Pages:\\s*(\\d+)" (get-output-string out)))
      ;; One for the canvas, three for the three advances, none for the hidden one.
      (check-equal? (and m (string->number (cadr m))) 4
                    "an advance is a page, and a hidden slide is not")])])

(printf "staged tests done\n")

(module+ main (void (test-log #:display? #t #:exit? #t)))
