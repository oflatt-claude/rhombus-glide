#lang racket/base
;; Every action a slideshow editor offers, walked through one at a time.
;;
;; The list is taken from the editors' own surfaces -- PowerPoint's ribbon
;; (Home, Insert, Design), its Format pane (Fill, Line, Size, Position, Text
;; Box), and Keynote's inspector -- rather than from the file format, which
;; `coverage.rkt` covers from the other side. Between them: one asks what a
;; deck can say, this asks what a person can do.
;;
;; Each action lands in one of four places, and the fourth is why this file
;; exists:
;;
;;   applied   written into the program
;;   reported  refused, with a reason, and nothing written
;;   noted     seen, and not an edit -- what a deck says when it says nothing
;;   ignored   deliberately invisible, like a transition
;;
;; and *silence*, which is none of those: the deck and the program disagreeing
;; with nobody told. Every action here asserts which of the four it is, so a
;; property that stops being compared fails rather than going quiet.
(require rackunit racket/list racket/string racket/file racket/path racket/format
         racket/runtime-path
         glide-pptx/sync glide-pptx/sync-state glide-pptx/export
         "deck-edit.rkt")

(define-runtime-path media-dir "media")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-actions"))
(delete-directory/files work #:must-exist? #f)
(make-directory* (build-path work "media"))
(copy-file (build-path media-dir "checker.png") (build-path work "media" "checker.png") #t)
(copy-file (build-path media-dir "gradient.png") (build-path work "media" "gradient.png") #t)

(define program (build-path work "deck.rhm"))
(define deck (build-path work "deck.pptx"))
(define workdir (build-path work "w"))

;; Enough to act on: a shape with an outline and one with neither fill nor
;; outline, a colour two shapes share, a line of two runs, a list of two
;; paragraphs, a picture, and a second slide.
(define program-text
  (string-join
   (list "#lang rhombus/and_meta"
         "import:"
         "  lib(\"glide-pptx/runtime.rhm\") open"
         "export:"
         "  all_slides"
         "def media = media_lookup(\"media\")"
         "def brand = hex(\"4472C4\")"
         "def slide_1 = slide_canvas("
         "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
         "  at(40.0, 40.0, ~tag: \"Outlined\","
         "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"ED7D31\"),"
         "                ~line: make_stroke(hex(\"203040\"), ~width: 2.0))),"
         "  at(200.0, 40.0, ~tag: \"Bare\","
         "     shape_pict(~width: 90.0, ~height: 60.0)),"
         "  at(340.0, 40.0, ~tag: \"Shared\","
         "     shape_pict(~width: 90.0, ~height: 60.0, ~fill: brand)),"
         "  at(480.0, 40.0, ~tag: \"Twin\","
         "     shape_pict(~width: 90.0, ~height: 60.0, ~fill: brand)),"
         "  at(40.0, 180.0, ~tag: \"Line\","
         "     textbox(~width: 400.0, ~height: 60.0, ~anchor: #'top,"
         "             para(~align: #'left, ~line_spacing: pair(#'percent, 1.0),"
         "                  run(\"hello \", ~font: \"Arial\", ~size: 24.0),"
         "                  run(\"world\", ~font: \"Arial\", ~size: 18.0)))),"
         "  at(40.0, 280.0, ~tag: \"List\","
         "     textbox(~width: 400.0, ~height: 120.0,"
         "             para(~align: #'left, run(\"first\", ~size: 18.0)),"
         "             para(~align: #'left, run(\"second\", ~size: 18.0)))),"
         "  at(40.0, 420.0, ~tag: \"Photo\","
         "     image_pict(media(\"checker.png\"), 160.0, 120.0))"
         ")"
         "def slide_2 = slide_canvas("
         "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
         "  at(60.0, 60.0, ~tag: \"Second\","
         "     shape_pict(~width: 100.0, ~height: 60.0, ~fill: hex(\"70AD47\")))"
         ")"
         "def all_slides = [slide_1, slide_2]"
         "")
   "\n"))

;; Laid down once. Each action restores these three files rather than exporting
;; again, which is the difference between a minute and five.
(define pristine (build-path work "pristine"))
(make-directory* pristine)
(display-to-file program-text program #:exists 'replace)
(picts->pptx (load-program-picts program) deck #:width 720.0 #:height 540.0)
(void (sync-once program deck #:workdir workdir))
(for ([f (in-list (list program deck (base-path-for program)))])
  (copy-file f (build-path pristine (file-name-from-path f)) #t))

(define (restore!)
  (for ([f (in-list (list program deck (base-path-for program)))])
    (copy-file (build-path pristine (file-name-from-path f)) f #t)))

(define counts (make-hash))

;; `outcome` is one of 'applied 'reported 'noted 'ignored. `says` is a regexp
;; the reason must match, for the ones that are refused; `writes` one the
;; program must match, for the ones that are not.
;; `settles-in` is how many passes it may take: one, except where a pass leaves
;; something for the next -- a group only has a drawing order once it exists.
(define (action name outcome edit! #:says [says #f] #:writes [writes #f]
                #:settles-in [settles-in 1])
  (restore!)
  (with-check-info (['action name])
    (define landed (edit!))
    (check-true (and landed #t) (format "~a: the edit was made" name))
    (define r (sync-once program deck #:workdir workdir #:atomic? #t))
    (define acted (filter (lambda (a) (not (eq? 'noted (sync-action-kind a))))
                          (sync-report-actions r)))
    (hash-update! counts outcome add1 0)
    (case outcome
      [(applied)
       (check-true (pair? acted) (format "~a: it was seen" name))
       (check-equal? (sync-report-skipped r) '()
                     (format "~a: refused ~s" name (map cdr (sync-report-skipped r))))
       (check-true (pair? (sync-report-applied r)) (format "~a: and written" name))
       (when writes
         (check-regexp-match writes (file->string program) (format "~a: what it wrote" name)))
       (let loop ([n 1])
         (define again (sync-once program deck #:workdir workdir #:atomic? #t))
         (cond
           [(null? (sync-report-actions again)) (void)]
           [(>= n settles-in)
            (fail (format "~a: still reporting ~s after ~a pass~a" name
                          (map sync-action-kind (sync-report-actions again))
                          n (if (= n 1) "" "es")))]
           [else (loop (add1 n))]))]
      [(reported)
       (check-true (pair? acted) (format "~a: it was seen" name))
       (check-equal? (sync-report-applied r) '() (format "~a: nothing written" name))
       (check-true (pair? (sync-report-skipped r)) (format "~a: and refused" name))
       (when says
         (check-regexp-match says (format "~a" (map cdr (sync-report-skipped r)))
                             (format "~a: with a reason" name)))]
      [(noted)
       (check-true (pair? (sync-report-notes r)) (format "~a: it was noted" name))
       (check-equal? (sync-report-skipped r) '() (format "~a: and did not stop a save" name))
       (check-equal? (sync-report-applied r) '() (format "~a: nor was it written" name))]
      [(ignored)
       (check-equal? (sync-report-actions r) '()
                     (format "~a: deliberately invisible" name))])))

(define (after tag rx to) (lambda () (edit-after-tag! deck 1 tag rx to)))

;; ------------------------------------------------------------ Home: the font

(action "bold" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" b=\"1\"")
        #:writes #rx"~bold: #true")
(action "italic" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" i=\"1\"")
        #:writes #rx"~italic: #true")
(action "underline" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" u=\"sng\"")
        #:writes #rx"~underline: #true")
(action "strikethrough" 'applied
        (after "Line" #px"sz=\"2400\"" "sz=\"2400\" strike=\"sngStrike\"")
        #:writes #rx"~strike: #true")
(action "font size" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"3600\"")
        #:writes #rx"~size: 36[.]0")
(action "typeface" 'applied (after "Line" #px"typeface=\"Arial\"" "typeface=\"Georgia\"")
        #:writes #rx"~font: \"Georgia\"")
(action "font colour" 'applied
        (after "Line" #px"<a:srgbClr val=\"000000\"/>" "<a:srgbClr val=\"CC0000\"/>")
        #:writes #rx"~color: hex[(]\"CC0000\"[)]")
(action "character spacing" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" spc=\"300\"")
        #:writes #rx"~spacing: 3[.]0")
(action "all caps" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" cap=\"all\"")
        #:writes #rx"~caps: #'all")
(action "superscript" 'applied (after "Line" #px"sz=\"2400\"" "sz=\"2400\" baseline=\"30000\"")
        #:writes #rx"~baseline: 0[.]3")
;; The second run of a line, which is one word of it.
;; The second run of a line, which is one word of it: it is the only one at
;; 18pt, so that is what names it.
(action "bold one word" 'applied (after "Line" #px"sz=\"1800\"" "sz=\"1800\" b=\"1\"")
        #:writes #rx"run[(]\"world\", ~font: \"Arial\", ~size: 18[.]0, ~bold: #true[)]")

;; ------------------------------------------------------- Home: the paragraph

(action "centre a paragraph" 'applied (after "List" #px"algn=\"l\"" "algn=\"ctr\"")
        #:writes #rx"~align: #'center")
(action "line spacing" 'applied
        (after "List" #px"<a:spcPct val=\"100000\"/>" "<a:spcPct val=\"150000\"/>")
        #:writes #rx"~line_spacing: pair[(]#'percent, 1[.]5[)]")
(action "space before a paragraph" 'applied
        (after "List" #px"</a:lnSpc>" "</a:lnSpc><a:spcBef><a:spcPts val=\"1200\"/></a:spcBef>")
        #:writes #rx"~space_before: 12[.]0")
(action "indent level" 'applied (after "List" #px"lvl=\"0\"" "lvl=\"1\"")
        #:writes #rx"~level: 1")
(action "hanging indent" 'applied
        (after "List" #px"marL=\"0\" indent=\"0\"" "marL=\"457200\" indent=\"-228600\"")
        #:writes #rx"~margin_left: 36[.]0")
(action "a bulleted list" 'applied
        (after "List" #px"<a:buNone/>" "<a:buChar char=\"•\"/>")
        #:writes #rx"~bullet: bullet[(]#'char")
(action "retype text" 'applied (after "List" #px"<a:t>first</a:t>" "<a:t>primary</a:t>")
        #:writes #rx"run[(]\"primary\"")

;; ----------------------------------------------------- Format pane: the text box

(action "vertical anchor" 'applied (after "Line" #px"anchor=\"t\"" "anchor=\"ctr\"")
        #:writes #rx"~anchor: #'center")
(action "word wrap off" 'applied (after "Line" #px"wrap=\"square\"" "wrap=\"none\"")
        #:writes #rx"~wrap: #false")
(action "shrink text on overflow" 'applied (after "Line" #px"<a:noAutofit/>" "<a:normAutofit/>")
        #:writes #rx"~autofit: #'shrink")
(action "text insets" 'applied (after "Line" #px"lIns=\"91440\"" "lIns=\"228600\"")
        #:writes #rx"~insets: insets[(]18[.]0")

;; --------------------------------------------------- Format pane: fill and line

(action "fill colour" 'applied
        (after "Outlined" #px"<a:srgbClr val=\"ED7D31\"/>" "<a:srgbClr val=\"C00000\"/>")
        #:writes #rx"~fill: hex[(]\"C00000\"[)]")
(action "fill opacity" 'applied
        (after "Outlined" #px"<a:srgbClr val=\"ED7D31\"/>"
               "<a:srgbClr val=\"ED7D31\"><a:alpha val=\"50000\"/></a:srgbClr>")
        #:writes #rx"~alpha: 0[.]5")
(action "no fill" 'applied
        (after "Outlined" #px"<a:solidFill><a:srgbClr val=\"ED7D31\"/></a:solidFill>" "<a:noFill/>")
        #:writes #rx"~fill: #false")
(action "a fill where there was none" 'applied
        (after "Bare" #px"<a:noFill/><a:ln>"
               "<a:solidFill><a:srgbClr val=\"00B050\"/></a:solidFill><a:ln>")
        #:writes #rx"~fill: hex[(]\"00B050\"[)]")
(action "a gradient fill" 'reported
        (after "Outlined" #px"<a:solidFill><a:srgbClr val=\"ED7D31\"/></a:solidFill>"
               (string-append "<a:gradFill><a:gsLst><a:gs pos=\"0\"><a:srgbClr val=\"FF0000\"/></a:gs>"
                              "<a:gs pos=\"100000\"><a:srgbClr val=\"0000FF\"/></a:gs></a:gsLst>"
                              "<a:lin ang=\"0\"/></a:gradFill>"))
        #:says #rx"gradient")
(action "outline colour" 'applied
        (after "Outlined" #px"<a:srgbClr val=\"203040\"/>" "<a:srgbClr val=\"FF0000\"/>")
        #:writes #rx"make_stroke[(]hex[(]\"FF0000\"[)]")
(action "outline width" 'applied (after "Outlined" #px"<a:ln w=\"25400\"" "<a:ln w=\"76200\"")
        #:writes #rx"~width: 6[.]0")
(action "a dashed outline" 'applied
        (after "Outlined" #px"</a:ln>" "<a:prstDash val=\"dash\"/></a:ln>")
        #:writes #rx"~dash: #'dash")
(action "a round line cap" 'applied (after "Outlined" #px"cap=\"flat\"" "cap=\"rnd\"")
        #:writes #rx"~cap: #'round")
(action "an arrowhead" 'applied
        (after "Outlined" #px"</a:ln>" "<a:tailEnd type=\"triangle\" w=\"med\" len=\"med\"/></a:ln>")
        #:writes #rx"~tail: line_end[(]#'triangle")
(action "no outline" 'applied
        (after "Outlined" #px"<a:ln w=\"[0-9]+\"[^>]*>.*?</a:ln>" "<a:ln><a:noFill/></a:ln>")
        #:writes #rx"~line: #false")
(action "an outline where there was none" 'applied
        (after "Bare" #px"<a:ln><a:noFill/></a:ln>"
               "<a:ln w=\"19050\"><a:solidFill><a:srgbClr val=\"FF0000\"/></a:solidFill></a:ln>")
        #:writes #rx"~line: make_stroke")
(action "recolour one of two that share a colour" 'reported
        (after "Shared" #px"<a:srgbClr val=\"4472C4\"/>" "<a:srgbClr val=\"7030A0\"/>")
        #:says #rx"shared with")

;; --------------------------------------------------- Format pane: size and place

(action "move" 'applied (lambda () (drag-in-deck! deck 1 "Outlined" 300.0 320.0))
        #:writes #rx"at[(]300[.]0, 320[.]0")
(action "resize" 'applied (lambda () (resize-in-deck! deck 1 "Outlined" 200.0 150.0))
        #:writes #rx"~width: 200[.]0")
(action "rotate" 'applied (lambda () (rotate-in-deck! deck 1 "Outlined" 30.0))
        #:writes #rx"~rotate: 30[.]0")
(action "flip" 'applied (after "Outlined" #px"<a:xfrm" "<a:xfrm flipH=\"1\"")
        #:writes #rx"~flip_h: #true")

;; ------------------------------------------------------------------ a picture

(action "crop a picture" 'applied
        (after "Photo" #px"</a:blip><a:stretch>"
               "</a:blip><a:srcRect l=\"10000\" t=\"5000\"/><a:stretch>")
        #:writes #rx"~crop: [[]0[.]1")
(action "fade a picture" 'applied
        (after "Photo" #px"></a:blip>" "><a:alphaModFix amt=\"40000\"/></a:blip>")
        #:writes #rx"~opacity: 0[.]4")
(action "swap a picture's image" 'applied
        (lambda ()
          (with-unpacked-deck deck
            (lambda (d)
              (for ([f (in-list (directory-list (build-path d "ppt" "media")))])
                (copy-file (build-path media-dir "gradient.png")
                           (build-path d "ppt" "media" f) #t))
              #t)))
        #:writes #rx"media[(]\"pic")

;; ------------------------------------------------------------- Home: arranging

(action "bring to front" 'applied (lambda () (bring-to-front! deck 1 "Outlined"))
        #:writes #rx"~tag: \"Outlined\"")
(action "group" 'applied
        (lambda () (group-in-deck! deck 1 "Outlined" "Bare" #:name "Pair"))
        ;; The group is drawn where its shapes were; the deck draws it last, and
        ;; that order is merged once the group exists to be moved.
        #:settles-in 2
        #:writes #rx"group_pict[(]")
(action "duplicate" 'applied (lambda () (duplicate-in-deck! deck 1 "Bare"))
        #:writes #rx"~tag: \"Bare [(]2[)]\"")
(action "delete a shape" 'applied (lambda () (delete-from-deck! deck 1 "Bare"))
        #:writes #rx"^(?!.*\"Bare\").*$")
(action "draw a new shape" 'applied
        (lambda () (and (add-shape-to-deck! deck 1 "Drawn") #t))
        #:writes #rx"~tag: \"Drawn\"")
(action "move a shape to another slide" 'applied
        (lambda () (move-element-to-slide! deck "Second" 2 1))
        #:writes #rx"~tag: \"Second\"")

;; ------------------------------------------------------------------- the slides

(action "background colour" 'applied
        (lambda () (edit-slide-part! deck 1 #px"<a:srgbClr val=\"FFFFFF\"/>"
                                    "<a:srgbClr val=\"102040\"/>"))
        #:writes #rx"~background: hex[(]\"102040\"[)]")
(action "reorder the slides" 'applied (lambda () (move-slide! deck 2 1))
        #:writes #rx"all_slides = [[]slide_2, slide_1[]]")
(action "delete a slide" 'applied (lambda () (delete-slide! deck 2))
        #:writes #rx"^(?!.*slide_2).*$")
;; Design -> Slide Size. A deck has one size however many slides it has, so
;; this is one edit on every canvas that states it.
(action "resize the deck" 'applied
        (lambda () (edit-part! deck "ppt/presentation.xml"
                               #px"<p:sldSz cx=\"[0-9]+\" cy=\"[0-9]+\""
                               "<p:sldSz cx=\"12192000\" cy=\"6858000\""))
        #:writes #rx"~width: 960[.]0")

;; ------------------------------------------------- what an editor does not mean

;; A transition belongs to the deck, not to what is on it, and animations are
;; the code's business. Neither is an edit to merge.
(action "a slide transition" 'ignored
        (lambda () (edit-slide-part! deck 1 #px"</p:cSld>"
                                    "</p:cSld><p:transition><p:fade/></p:transition>")))
;; A theme's colours are not what our shapes are painted with -- they state
;; their own -- so changing one changes nothing here.
(action "the theme's colours" 'ignored
        (lambda () (edit-part! deck "ppt/theme/theme1.xml" #px"<a:srgbClr val=\"4472C4\"/>"
                               "<a:srgbClr val=\"FF00FF\"/>")))
;; A run that states the defaults says what a deck says when it says nothing.
(action "text set to the defaults" 'noted
        (after "Line" #px"sz=\"2400\"" "sz=\"1800\""))

(printf "action tests done; ~a\n"
        (string-join (for/list ([k (in-list '(applied reported noted ignored))])
                       (format "~a ~a" (hash-ref counts k 0) k))
                     ", "))
