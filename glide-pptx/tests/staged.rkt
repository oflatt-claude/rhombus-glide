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
(require rackunit racket/file racket/list racket/path racket/system racket/port
         racket/string racket/runtime-path
         glide-pptx/sync glide-pptx/sync-state glide-pptx/export
         "deck-edit.rkt")

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
     "  // For `sliderec_title`, which is what `a` and `s` navigate by and is"
     "  // not something `slideshow/main` hands out."
     "  lib(\"slideshow/core.rkt\") as core"
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
     "// Counted as the defaults leave them -- a cut between slides and no fade"
     "// up from blank -- so that what is counted is the slides themselves and"
     "// not the frames they would be played through."
     "fun emitted():"
     "  recur count(n = 0):"
     "    if ss.#{most-recent-slide}()"
     "    | block:"
     "        ss.#{retract-most-recent-slide}()"
     "        count(n + 1)"
     "    | n"
     "show_slides([canvas, canvas, hidden], ~width: w, ~height: h)"
     "println(\"stills \" +& emitted())"
     "// A slide that names a transition still gets one: the default is what a"
     "// slide gets when it says nothing, not a veto."
     "show_slides([canvas, panned], ~width: w, ~height: h)"
     "println(\"panned \" +& emitted())"
     "show_slides([with_stages], ~width: w, ~height: h)"
     "println(\"stages \" +& emitted())"
     "// The title every page carries, which `a` and `s` group by. Collected"
     "// while retracting, so it also clears the slides it counted."
     "fun titles():"
     "  recur go(acc = []):"
     "    def s = ss.#{most-recent-slide}()"
     "    if s"
     "    | block:"
     "        ss.#{retract-most-recent-slide}()"
     "        go([core.#{sliderec-title}(s), & acc])"
     "    | acc"
     "show_slides([with_stages, canvas], ~width: w, ~height: h)"
     "def ts = titles()"
     "def groups = for Map (t in ts): values(t, #true)"
     "println(\"titles \" +& ts.length() +& \" pages \" +& groups.length() +& \" groups\")"
     "// And the title is never drawn: a slide handed one is the same picture as"
     "// a slide handed none."
     "do_staged_slide(canvas, ~layout: #'center)"
     "def plain = ss.#{slide->pict}(ss.#{most-recent-slide}())"
     "ss.#{retract-most-recent-slide}()"
     "do_staged_slide(canvas, ~title: \"Zebra Zebra Zebra\", ~layout: #'center)"
     "def titled = ss.#{slide->pict}(ss.#{most-recent-slide}())"
     "ss.#{retract-most-recent-slide}()"
     "println(\"drawn \" +& (Pict.from_handle(plain).height"
     "                      == Pict.from_handle(titled).height))"
     "// And with the reveal turned on, a still slide is faded up rather than cut"
     "// to, which costs it an advance."
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
   ;; Two slides, one advance each: by default a slide is cut to, not faded up,
   ;; and not panned to either. A deck behaves the way it did in PowerPoint
   ;; unless the talk asks for something else.
   (check-regexp-match #rx"stills 2" out "a hidden slide is not shown, and a still slide is one slide")
   (check-regexp-match #px"panned ([3-9]|[0-9][0-9]+)" out
                       "and a slide that asks to be panned to is played into")
   ;; An animated epoch is played as several pages, so this counts pages and not
   ;; presses. The number is exact because the hold is what is being checked:
   ;; the last epoch of an animated slide plays as the transition off it, so the
   ;; show sustains the final frame rather than letting the move-on eat it, and
   ;; that is one page more than the same slide shown without it -- 24 here,
   ;; where it would be 23. A still slide is not held, which is `stills 2`.
   (check-regexp-match #rx"stages 24" out
                       "a slide with stages is played out, with its last frame held")
   (check-regexp-match #px"revealed ([3-9]|[0-9][0-9]+)" out
                       "and with the reveal on, a still slide is faded up rather than cut to")

   ;; ------------------------------------------------------- a and s navigate
   ;; `s` skips to the next slide with a different title and `a` back to the
   ;; start of the previous group, so the title is what decides how far a press
   ;; of either goes. Every slide had the same one -- `#false` -- and both keys
   ;; ran to the end of the talk. Now an animated slide's pages share one title
   ;; and the next slide has its own: two groups over more than two pages, which
   ;; is `s` stepping a slide at a time rather than a frame at a time.
   (let ([m (regexp-match #px"titles ([0-9]+) pages ([0-9]+) groups" out)])
     (check-true (and m #t) "the pages say what they are titled")
     (when m
       (check-equal? (string->number (caddr m)) 2
                     "an animated slide and a still one are two groups")
       (check-true (> (string->number (cadr m)) 2)
                   "over more pages than that, which is what the grouping is for")))
   ;; Set for navigating by, not for drawing: a converted deck carries its own
   ;; title inside the page and a second one over the top is not the deck.
   (check-regexp-match #rx"drawn #true" out
                       "a slide handed a title is the same picture as one handed none")

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

;; A program can be loaded twice in one process.
;;
;; `load-program-picts` reads a program in a namespace of its own, because a
;; second `dynamic-require` in the same one hands back the instance it already
;; has and the merge would never see its own patch. But a talk whose helpers
;; import slideshow pulls in racket/gui, which cannot be instantiated twice in a
;; process -- and the watch loop loads the program again on every save. So the
;; second save died on a program that drew through slideshow, which is to say on
;; a talk.
(cond
  [(not display?)
   (printf "no display and no xvfb-run; loading twice is not checked\n")]
  [else
   (define dir (build-path work "twice"))
   (make-directory* dir)
   (define program (build-path dir "p.rhm"))
   (call-with-output-file program #:exists 'replace
     (lambda (o)
       (write-string
        (string-join
         (list "#lang rhombus/and_meta"
               "import:"
               "  lib(\"glide-pptx/runtime.rhm\") open"
               "  // The import that pulls in the GUI, as a talk's helpers do."
               "  slideshow as ss"
               ""
               "export: all_slides"
               "def slide_1 = slide_canvas(~width: 320.0, ~height: 240.0)"
               "def all_slides = [slide_1]")
         "\n")
        o)))
   (define driver (build-path dir "twice.rkt"))
   (call-with-output-file driver #:exists 'replace
     (lambda (o)
       (write-string
        (format "#lang racket/base\n(require glide-pptx/sync)\n~a\n~a\n~a\n"
                (format "(define p ~s)" (path->string program))
                "(printf \"first ~a\\n\" (length (load-program-picts p)))"
                "(printf \"second ~a\\n\" (length (load-program-picts p)))")
        o)))
   (define out (open-output-string))
   (define err (open-output-string))
   ;; With arguments on the command line, because `raco glide talk.rhm --app none`
   ;; has some -- and a program that imports slideshow reads them when it loads,
   ;; and refuses anything that is not a single module file. They are this
   ;; command's arguments, not the program's, and the program must not see them.
   (define ok?
     (parameterize ([current-output-port out] [current-error-port err])
       (if xvfb
           (system* xvfb "-a" racket-exe (path->string driver) "--app" "none")
           (system* racket-exe (path->string driver) "--app" "none"))))
   (check-true ok? (format "the program loaded twice: ~a" (get-output-string err)))
   (check-regexp-match #rx"first 1" (get-output-string out) "the first load found the slide")
   (check-regexp-match #rx"second 1" (get-output-string out)
                       "and so did the second, in the same process")])

;; --------------------------------------------- one slide per stage, on request
;;
;; A slide given stages is one page per stage in the show -- slideshow's own
;; doing -- and a deck that holds one flattened picture of it cannot be stepped
;; through at all. `--stages` asks for the pages instead, and what arrives has
;; to be slides with shapes on them: `export --slideshow` already produced one
;; picture per advance, which is a deck nobody can edit or print sharply.
;;
;; Off by default, and it has to stay off for the deck a session keeps: an edit
;; has to have one place to go, and a slide that appears four times is a shape
;; dragged on one of them and three that disagree.
(let ()
  (define program (build-path work "stages.rhm"))
  (display-to-file
   (string-append
    PROLOGUE
    (string-join
     (list "export: all_slides"
           "def one = glide.slide_canvas("
           "  ~width: w, ~height: h,"
           "  glide.at(20.0, 20.0, ~tag: \"Box\","
           "           glide.shape_pict(~width: 60.0, ~height: 40.0,"
           "                            ~fill: glide.hex(\"4472C4\"))))"
           "def two:"
           "  def base = Pict.from_handle(one)"
           "  switch(base, animate(fun (t): base.alpha(t)))"
           "def all_slides = [one, two]")
     "
"))
   program #:exists 'replace)
  (define tags
    (for/list ([st (in-list (program-slide-states program))])
      (for/list ([e (in-list (slide-state-elements st))]) (el-state-tag e))))
  (check-equal? tags '(("Box") ("Box"))
                "settled, a staged slide is one slide")
  (set-stage-slides! #t)
  (define staged
    (for/list ([st (in-list (program-slide-states program))])
      (for/list ([e (in-list (slide-state-elements st))]) (el-state-tag e))))
  (set-stage-slides! #f)
  (check-equal? staged '(("Box") ("Box") ("Box"))
                "with stages, it is one slide per stage -- with its shapes, not a picture of them")
  ;; And the number every page of one slide carries is that slide's own, which
  ;; is what the show draws: the number is composed before slideshow makes a
  ;; page of each stage.
  (set-stage-slides! #t)
  (ask-slide-numbers! #t)
  (define numbered
    (for/list ([st (in-list (program-slide-states program))])
      (length (slide-state-elements st))))
  (ask-slide-numbers! #f)
  (set-stage-slides! #f)
  (check-equal? numbered '(2 2 2) "each page gains the number in its corner"))

;; ------------------------------------------- editing a slide stage by stage
;;
;; With one slide of the deck per stage, a shape can be put where it belongs on
;; the stage it appears on. Three things have to hold for that to be worth
;; anything:
;;
;;   * the deck has a slide per stage, and each of them can be read;
;;   * an edit made on the third stage is written to the one `at` form that
;;     draws the shape -- `all_slides` names the animation, not the canvas it
;;     was built from, so the scope is found one definition further in;
;;   * the deck is written again from the program afterwards. One form draws the
;;     shape on every stage, so the stages that were not edited still show what
;;     they showed, and without that they report the same difference back for
;;     ever.
(let ()
  (define program (build-path work "stage-edit.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "  pict as pc"
          "export: all_slides"
          "def canvas = slide_canvas("
          "  ~width: 320.0, ~height: 240.0,"
          "  at(20.0, 20.0, ~tag: \"Box\","
          "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))),"
          "  at(120.0, 120.0, ~tag: \"Later\","
          "     shape_pict(~width: 50.0, ~height: 30.0, ~fill: hex(\"ED7D31\"))))"
          "def staged:"
          "  def base = pc.Pict.from_handle(canvas)"
          "  pc.switch(base, pc.animate(fun (t): base.alpha(t)))"
          "def all_slides = [staged]")
    "\n")
   program #:exists 'replace)
  (set-stage-slides! #t)
  (define picts (load-program-picts program))
  (check-equal? (length picts) 2 "a slide of two stages is two slides of the deck")
  (define-values (sites scopes slide-sites layout) (find-program-sites program))
  (check-equal? scopes '(canvas)
                "and `all_slides` is followed through the animation to the canvas")
  (define deck (build-path work "stage-edit.pptx"))
  (define w (build-path work "stage-edit-work"))
  (picts->pptx picts deck)
  (void (sync-once program deck #:workdir w))
  (check-true (drag-in-deck! deck 2 "Later" 200.0 60.0)
              "something is dragged on the second stage")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(moved)
                "the drag is written")
  (check-regexp-match #rx"at[(]200[.]0, 60[.]0, ~tag: \"Later\""
                      (file->string program)
                      "to the one `at` form that draws it")
  (check-true (sync-report-deck-behind? r)
              "and the deck is behind, so the other stages are written again")
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (sync-report-actions (sync-once program deck #:workdir w #:atomic? #t)) '()
                "after which there is nothing left to merge")
  (set-stage-slides! #f))

;; ------------------------------------------ a shape drawn on a later stage
;;
;; Drawing a box on the third stage of a slide means a box that appears there
;; and not before. The slide's canvas cannot say that -- a canvas is one still
;; picture, and the staging is applied to it from outside -- so what is written
;; is a layer over the slide, in a `from_stage` wrapped around its entry in
;; `all_slides`. The `at` form inside it keeps its tag, which is what makes the
;; next drag land: a shape this wrote is a shape like any other.
(let ()
  (define program (build-path work "stage-add.rhm"))
  (define (fresh!)
    (display-to-file
     (string-join
      (list "#lang rhombus/and_meta"
            "import:"
            "  lib(\"glide-pptx/runtime.rhm\") open"
            "  pict as pc"
            "export: all_slides"
            "def canvas = slide_canvas("
            "  ~width: 320.0, ~height: 240.0,"
            "  at(20.0, 20.0, ~tag: \"Box\","
            "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"4472C4\"))))"
            "def staged:"
            "  def base = pc.Pict.from_handle(canvas)"
            "  pc.switch(base, pc.animate(fun (t): base.alpha(t)))"
            "def all_slides = [staged]")
      "\n")
     program #:exists 'replace))
  (fresh!)
  (set-stage-slides! #t)
  (define deck (build-path work "stage-add.pptx"))
  (define w (build-path work "stage-add-work"))
  (picts->pptx (load-program-picts program) deck)
  (void (sync-once program deck #:workdir w))
  (check-equal? (add-shape-to-deck! deck 2 "Drawn late" #:x 40.0 #:y 200.0
                                    #:width 60.0 #:height 20.0)
                "Drawn late"
                "a shape is drawn on the second stage")
  (define r (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r)) '(added)
                "and written")
  (check-regexp-match #rx"from_stage[(]2, staged," (file->string program)
                      "as a layer over the slide, appearing from that stage")
  (define (tags-on i)
    (for/first ([st (in-list (program-slide-states program))]
                #:when (= i (slide-state-index st)))
      (for/list ([e (in-list (slide-state-elements st))]) (el-state-tag e))))
  (check-equal? (tags-on 1) '("Box") "the first stage does not hold it")
  (check-equal? (tags-on 2) '("Box" "Drawn late") "and the second does")
  (picts->pptx (load-program-picts program) deck)
  (check-equal? (sync-report-actions (sync-once program deck #:workdir w #:atomic? #t)) '()
                "the deck written from the program has nothing to merge")
  ;; And it can be dragged, like anything else with a tag and an `at`.
  (check-true (drag-in-deck! deck 2 "Drawn late" 90.0 150.0) "it is dragged")
  (define r2 (sync-once program deck #:workdir w #:atomic? #t))
  (check-equal? (map sync-action-kind (sync-report-applied r2)) '(moved)
                "and the drag is written")
  (check-regexp-match #rx"at[(]90[.]0, 150[.]0, ~tag: \"Drawn late\""
                      (file->string program)
                      "to the `at` form the layer holds")
  (set-stage-slides! #f))

(printf "staged tests done\n")

(module+ main (void (test-log #:display? #t #:exit? #t)))
