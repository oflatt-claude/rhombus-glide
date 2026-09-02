#lang racket/base
;; display list -> .pptx
;;
;; Every item becomes a real PowerPoint object: a rectangle is a `prstGeom`
;; rectangle, a path is a `custGeom`, a string is a text box you can retype, a
;; bitmap is a picture. Nothing is rasterized that does not have to be.
;;
;; The package is written from scratch with one master, one blank layout and one
;; theme, and every property stated explicitly on the slide. That needs no
;; placeholder or inheritance machinery, and it means our own importer can read
;; back what we write, which is what makes the export self-testing.
(require racket/list racket/string racket/math racket/file racket/path
         racket/format racket/class racket/draw
         file/zip
         "draw-ir.rkt" (prefix-in ir: "ir.rkt"))
(provide display-pages->pptx current-write-warnings)

(define current-write-warnings (make-parameter #f))
(define (warn! fmt . args)
  (define b (current-write-warnings))
  (when b (set-box! b (cons (apply format fmt args) (unbox b)))))

;; ------------------------------------------------------------------- units

(define (emu v) (inexact->exact (round (* 12700.0 (exact->inexact v)))))
(define (angle-60k deg)
  (inexact->exact (round (* 60000.0 (let loop ([d (exact->inexact deg)])
                                      (cond [(< d 0) (loop (+ d 360.0))]
                                            [(>= d 360.0) (loop (- d 360.0))]
                                            [else d]))))))
(define (pct v) (inexact->exact (round (* 100000.0 (max 0.0 (min 1.0 v))))))

(define (hex-of c)
  (define (two n)
    (define s (number->string (max 0 (min 255 (inexact->exact (round n)))) 16))
    (string-upcase (if (= 1 (string-length s)) (string-append "0" s) s)))
  (string-append (two (rgba*-r c)) (two (rgba*-g c)) (two (rgba*-b c))))

;; A color, with an alpha child only when it is not opaque.
(define (clr c)
  (if (>= (rgba*-a c) 0.999)
      (format "<a:srgbClr val=\"~a\"/>" (hex-of c))
      (format "<a:srgbClr val=\"~a\"><a:alpha val=\"~a\"/></a:srgbClr>"
              (hex-of c) (pct (rgba*-a c)))))

(define (xml-escape s)
  (for/fold ([s s]) ([p (in-list '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")
                                   ("\"" . "&quot;")))])
    (string-replace s (car p) (cdr p))))

;; ------------------------------------------------------------------- paint

(define (fill-xml f)
  (cond
    [(not f) "<a:noFill/>"]
    [(fill:solid? f) (format "<a:solidFill>~a</a:solidFill>" (clr (fill:solid-color f)))]
    [(fill:linear? f)
     (define dx (- (fill:linear-x1 f) (fill:linear-x0 f)))
     (define dy (- (fill:linear-y1 f) (fill:linear-y0 f)))
     ;; `ang` is clockwise from the positive x axis, and our y already points
     ;; down, so atan2 gives it directly.
     (format "<a:gradFill rotWithShape=\"0\"><a:gsLst>~a</a:gsLst><a:lin ang=\"~a\" scaled=\"0\"/></a:gradFill>"
             (stops-xml (fill:linear-stops f))
             (angle-60k (radians->degrees (atan dy dx))))]
    [(fill:radial? f)
     (format (string-append "<a:gradFill rotWithShape=\"0\"><a:gsLst>~a</a:gsLst>"
                            "<a:path path=\"circle\"><a:fillToRect l=\"50000\" t=\"50000\""
                            " r=\"50000\" b=\"50000\"/></a:path></a:gradFill>")
             (stops-xml (fill:radial-stops f)))]
    [else "<a:noFill/>"]))

;; DrawingML needs at least two stops and them in order.
(define (stops-xml stops)
  (define ss (sort (if (< (length stops) 2)
                       (list (list 0.0 (second (first stops)))
                             (list 1.0 (second (first stops))))
                       stops)
                   < #:key first))
  (apply string-append
         (for/list ([s (in-list ss)])
           (format "<a:gs pos=\"~a\">~a</a:gs>" (pct (first s)) (clr (second s))))))

(define dash-xml
  (hash 'solid "" 'dash "<a:prstDash val=\"dash\"/>"
        'dot "<a:prstDash val=\"sysDot\"/>" 'dash-dot "<a:prstDash val=\"dashDot\"/>"))

(define (line-xml p)
  (cond
    [(not p) "<a:ln><a:noFill/></a:ln>"]
    [else
     ;; A hairline has no width in DrawingML; the thinnest real line stands in.
     (define w (if (zero? (pen*-width p)) 0.75 (pen*-width p)))
     (format "<a:ln w=\"~a\" cap=\"~a\"><a:solidFill>~a</a:solidFill>~a<a:~a/></a:ln>"
             (emu w)
             (case (pen*-cap p) [(round) "rnd"] [(square) "sq"] [else "flat"])
             (clr (pen*-color p))
             (hash-ref dash-xml (pen*-dash p) "")
             (case (pen*-join p) [(round) "round"] [(bevel) "bevel"] [else "miter"]))]))

;; ------------------------------------------------------------------ shapes

;; An element's tag goes in both `name`, which PowerPoint shows in its Selection
;; Pane, and `descr`, the alt text. Neither is guaranteed to survive an editor
;; -- LibreOffice destroys both -- so they are a convenience for matching later,
;; not something to rely on.
(define (nv-xml id name #:tag [tag #f])
  (format (string-append "<p:nvSpPr><p:cNvPr id=\"~a\" name=\"~a\"~a/>"
                         "<p:cNvSpPr/><p:nvPr/></p:nvSpPr>")
          id (xml-escape (or tag name))
          (if tag (format " descr=\"glide-pptx:~a\"" (xml-escape tag)) "")))

(define (xfrm-xml x y w h rot [fh #f] [fv #f])
  (format "<a:xfrm~a~a~a><a:off x=\"~a\" y=\"~a\"/><a:ext cx=\"~a\" cy=\"~a\"/></a:xfrm>"
          (if (zero? rot) "" (format " rot=\"~a\"" (angle-60k rot)))
          (if fh " flipH=\"1\"" "") (if fv " flipV=\"1\"" "")
          (emu x) (emu y) (max 1 (emu w)) (max 1 (emu h))))

(define EMPTY-TX "<p:txBody><a:bodyPr/><a:lstStyle/><a:p/></p:txBody>")

(define (shape-xml id name geom x y w h rot fill pen)
  (format "<p:sp>~a<p:spPr>~a~a~a~a</p:spPr>~a</p:sp>"
          (nv-xml id name) (xfrm-xml x y w h rot) geom
          (fill-xml fill) (line-xml pen) EMPTY-TX))

(define (rect-item-xml id i)
  (define r (it:rect-radius i))
  (define w (it:rect-w i)) (define h (it:rect-h i))
  (define geom
    (if (and (positive? r) (positive? (min w h)))
        ;; roundRect's adjustment is a fraction of the smaller side.
        (format "<a:prstGeom prst=\"roundRect\"><a:avLst><a:gd name=\"adj\" fmla=\"val ~a\"/></a:avLst></a:prstGeom>"
                (pct (min 0.5 (/ r (min w h)))))
        "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>"))
  (shape-xml id (format "Rectangle ~a" id) geom
             (it:rect-x i) (it:rect-y i) w h (it:rect-rot i)
             (it:rect-fill i) (it:rect-pen i)))

(define (ellipse-item-xml id i)
  (shape-xml id (format "Oval ~a" id) "<a:prstGeom prst=\"ellipse\"><a:avLst/></a:prstGeom>"
             (it:ellipse-x i) (it:ellipse-y i) (it:ellipse-w i) (it:ellipse-h i)
             (it:ellipse-rot i) (it:ellipse-fill i) (it:ellipse-pen i)))

;; ------------------------------------------------------------------- paths

;; A custGeom's coordinates live in the path's own space, which the shape's
;; extent then maps, so they are written relative to the item's bounding box.
(define (path-item-xml id i)
  (define segs (it:path-segs i))
  (define-values (bx by bw bh) (apply values (segs-bounds segs)))
  (define w (max bw 0.001)) (define h (max bh 0.001))
  (shape-xml id (format "Freeform ~a" id) (path-geom-xml segs bx by w h)
             bx by w h 0.0 (it:path-fill i) (it:path-pen i)))

;; Path coordinates live in the path's own space, which the shape's extent maps,
;; so they are written relative to the bounding box.
(define (path-geom-xml segs bx by w h)
  (define (px v) (emu (- v bx)))
  (define (py v) (emu (- v by)))
  (define body
    (apply string-append
           (for/list ([s (in-list segs)])
             (cond
               [(seg:move? s) (format "<a:moveTo><a:pt x=\"~a\" y=\"~a\"/></a:moveTo>"
                                      (px (seg:move-x s)) (py (seg:move-y s)))]
               [(seg:line? s) (format "<a:lnTo><a:pt x=\"~a\" y=\"~a\"/></a:lnTo>"
                                      (px (seg:line-x s)) (py (seg:line-y s)))]
               [(seg:curve? s)
                (format (string-append "<a:cubicBezTo><a:pt x=\"~a\" y=\"~a\"/>"
                                       "<a:pt x=\"~a\" y=\"~a\"/><a:pt x=\"~a\" y=\"~a\"/>"
                                       "</a:cubicBezTo>")
                        (px (seg:curve-x1 s)) (py (seg:curve-y1 s))
                        (px (seg:curve-x2 s)) (py (seg:curve-y2 s))
                        (px (seg:curve-x s)) (py (seg:curve-y s)))]
               [else "<a:close/>"]))))
  (format (string-append "<a:custGeom><a:avLst/><a:gdLst/><a:ahLst/><a:cxnLst/>"
                         "<a:rect l=\"0\" t=\"0\" r=\"~a\" b=\"~a\"/>"
                         "<a:pathLst><a:path w=\"~a\" h=\"~a\">~a</a:path></a:pathLst>"
                         "</a:custGeom>")
          (emu w) (emu h) (emu w) (emu h) body))

;; -------------------------------------------------------------------- text

;; A one-line text box with zero insets and no wrapping, sized to the measured
;; string, so PowerPoint puts the first baseline where the pict had it.
(define (text-item-xml id i)
  (define sz (max 100 (inexact->exact (round (* 100 (it:text-size i))))))
  (define rpr
    (format (string-append "<a:rPr lang=\"en-US\" sz=\"~a\"~a~a~a dirty=\"0\">"
                           "<a:solidFill>~a</a:solidFill>"
                           "<a:latin typeface=\"~a\"/><a:cs typeface=\"~a\"/></a:rPr>")
            sz
            (if (it:text-bold? i) " b=\"1\"" "")
            (if (it:text-italic? i) " i=\"1\"" "")
            (if (it:text-underline? i) " u=\"sng\"" "")
            (clr (it:text-color i))
            (xml-escape (it:text-face i)) (xml-escape (it:text-face i))))
  (define body
    (format (string-append "<p:txBody><a:bodyPr wrap=\"none\" lIns=\"0\" tIns=\"0\""
                           " rIns=\"0\" bIns=\"0\" anchor=\"t\" anchorCtr=\"0\">"
                           "<a:noAutofit/></a:bodyPr><a:lstStyle/>"
                           "<a:p><a:pPr algn=\"l\" marL=\"0\" indent=\"0\"><a:lnSpc>"
                           "<a:spcPct val=\"100000\"/></a:lnSpc><a:buNone/></a:pPr>"
                           "<a:r>~a<a:t>~a</a:t></a:r></a:p></p:txBody>")
            rpr (xml-escape (it:text-str i))))
  ;; A little horizontal slack, because PowerPoint measures the string itself and
  ;; a hair more than we did would otherwise clip it.
  (format "<p:sp>~a<p:spPr>~a<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom><a:noFill/><a:ln><a:noFill/></a:ln></p:spPr>~a</p:sp>"
          (nv-xml id (format "Text ~a" id))
          (xfrm-xml (it:text-x i) (it:text-y i)
                    (+ 2.0 (it:text-w i)) (it:text-h i) (it:text-rot i))
          body))

;; ------------------------------------------------------- rich text bodies

;; Writes an `ir:text-body` as paragraphs and runs, so PowerPoint lays the text
;; out and can reflow it. This is the inverse of the import side's text parsing,
;; and it is what a text box exported semantically gets instead of one
;; unwrappable box per drawn word.
(define (txbody-xml body)
  (define ins (ir:text-body-insets body))
  (format (string-append "<p:txBody><a:bodyPr wrap=\"~a\" lIns=\"~a\" tIns=\"~a\""
                         " rIns=\"~a\" bIns=\"~a\" anchor=\"~a\" anchorCtr=\"~a\">"
                         "~a</a:bodyPr><a:lstStyle/>~a</p:txBody>")
          (if (ir:text-body-wrap? body) "square" "none")
          (emu (ir:insets-l ins)) (emu (ir:insets-t ins))
          (emu (ir:insets-r ins)) (emu (ir:insets-b ins))
          (case (ir:text-body-anchor body)
            [(center) "ctr"] [(bottom) "b"] [else "t"])
          (if (ir:text-body-anchor-center? body) "1" "0")
          (case (ir:text-body-autofit body)
            [(shrink) "<a:normAutofit/>"] [(grow) "<a:spAutoFit/>"]
            [else "<a:noAutofit/>"])
          (apply string-append
                 (for/list ([p (in-list (ir:text-body-paras body))]) (para-xml p)))))

(define (para-xml p)
  (define b (ir:para-bullet p))
  (define spacing (ir:para-line-spacing p))
  (format "<a:p><a:pPr algn=\"~a\" lvl=\"~a\" marL=\"~a\" indent=\"~a\">~a~a~a~a</a:pPr>~a</a:p>"
          (case (ir:para-align p)
            [(center) "ctr"] [(right) "r"] [(justify) "just"] [else "l"])
          (ir:para-level p)
          (emu (ir:para-margin-left p)) (emu (ir:para-indent p))
          (if (eq? 'points (car spacing))
              (format "<a:lnSpc><a:spcPts val=\"~a\"/></a:lnSpc>"
                      (inexact->exact (round (* 100 (cdr spacing)))))
              (format "<a:lnSpc><a:spcPct val=\"~a\"/></a:lnSpc>" (pct (cdr spacing))))
          (if (zero? (ir:para-space-before p)) ""
              (format "<a:spcBef><a:spcPts val=\"~a\"/></a:spcBef>"
                      (inexact->exact (round (* 100 (ir:para-space-before p))))))
          (if (zero? (ir:para-space-after p)) ""
              (format "<a:spcAft><a:spcPts val=\"~a\"/></a:spcAft>"
                      (inexact->exact (round (* 100 (ir:para-space-after p))))))
          (bullet-xml b)
          (apply string-append
                 (for/list ([r (in-list (ir:para-runs p))]) (run-xml r)))))

(define (bullet-xml b)
  (case (ir:bullet-kind b)
    [(char) (format "~a~a<a:buChar char=\"~a\"/>"
                    (if (ir:bullet-color b)
                        (format "<a:buClr>~a</a:buClr>"
                                (clr (ir-rgba (ir:bullet-color b)))) "")
                    (if (ir:bullet-font b)
                        (format "<a:buFont typeface=\"~a\"/>" (xml-escape (ir:bullet-font b)))
                        "")
                    (xml-escape (or (ir:bullet-char b) "\u2022")))]
    [(number) (format "<a:buAutoNum type=\"~a\"/>"
                      (xml-escape (or (ir:bullet-char b) "arabicPeriod")))]
    [else "<a:buNone/>"]))

(define (run-xml r)
  (format (string-append "<a:r><a:rPr lang=\"en-US\" sz=\"~a\"~a~a~a~a~a~a dirty=\"0\">"
                         "<a:solidFill>~a</a:solidFill>"
                         "<a:latin typeface=\"~a\"/><a:cs typeface=\"~a\"/></a:rPr>"
                         "<a:t>~a</a:t></a:r>")
          (max 100 (inexact->exact (round (* 100 (ir:trun-size r)))))
          (if (ir:trun-bold? r) " b=\"1\"" "")
          (if (ir:trun-italic? r) " i=\"1\"" "")
          (if (ir:trun-underline? r) " u=\"sng\"" "")
          (if (ir:trun-strike? r) " strike=\"sngStrike\"" "")
          (if (zero? (ir:trun-spacing r)) ""
              (format " spc=\"~a\"" (inexact->exact (round (* 100 (ir:trun-spacing r))))))
          (if (zero? (ir:trun-baseline r)) ""
              (format " baseline=\"~a\"" (pct (ir:trun-baseline r))))
          (clr (ir-rgba (ir:trun-color r)))
          (xml-escape (ir:trun-family r)) (xml-escape (ir:trun-family r))
          (xml-escape (ir:trun-text r))))

(define (ir-rgba c)
  (rgba* (ir:rgba-r c) (ir:rgba-g c) (ir:rgba-b c) (ir:rgba-a c)))

(define (body-or-empty body)
  (if (and body (not (ir:text-body-empty? body))) (txbody-xml body) EMPTY-TX))

;; ---------------------------------------------------- items with structure

;; A named preset, so PowerPoint draws its own version of the shape and the
;; text inside stays reflowable.
(define (preset-item-xml id i)
  (define adjust (it:preset-adjust i))
  (define geom
    (format "<a:prstGeom prst=\"~a\"><a:avLst>~a</a:avLst></a:prstGeom>"
            (xml-escape (it:preset-name i))
            (apply string-append
                   (for/list ([a (in-list adjust)])
                     (format "<a:gd name=\"~a\" fmla=\"~a\"/>"
                             (xml-escape (car a)) (xml-escape (cdr a)))))))
  (format "<p:sp>~a<p:spPr>~a~a~a~a</p:spPr>~a</p:sp>"
          (nv-xml id (format "Shape ~a" id) #:tag (it:preset-tag i))
          (xfrm-xml (it:preset-x i) (it:preset-y i) (it:preset-w i) (it:preset-h i)
                    (it:preset-rot i) (it:preset-flip-h? i) (it:preset-flip-v? i))
          geom (fill-xml (it:preset-fill i)) (line-xml (it:preset-pen i))
          (body-or-empty (it:preset-body i))))

(define (textbox-item-xml id i)
  (format (string-append "<p:sp>~a<p:spPr>~a<a:prstGeom prst=\"rect\"><a:avLst/>"
                         "</a:prstGeom><a:noFill/><a:ln><a:noFill/></a:ln></p:spPr>~a</p:sp>")
          (nv-xml id (format "TextBox ~a" id) #:tag (it:textbox-tag i))
          (xfrm-xml (it:textbox-x i) (it:textbox-y i) (it:textbox-w i) (it:textbox-h i)
                    (it:textbox-rot i))
          (body-or-empty (it:textbox-body i))))

;; Custom geometry keeps its drawn path but not its name, and still carries text.
(define (shape-path-item-xml id i)
  (define segs (it:shape-path-segs i))
  (define-values (bx by bw bh) (apply values (segs-bounds segs)))
  (format "<p:sp>~a<p:spPr>~a~a~a~a</p:spPr>~a</p:sp>"
          (nv-xml id (format "Freeform ~a" id) #:tag (it:shape-path-tag i))
          (xfrm-xml bx by (max bw 0.001) (max bh 0.001) 0.0)
          (path-geom-xml segs bx by (max bw 0.001) (max bh 0.001))
          (fill-xml (it:shape-path-fill i)) (line-xml (it:shape-path-pen i))
          (body-or-empty (it:shape-path-body i))))

;; A picture from a file on disk keeps its original encoding rather than being
;; round-tripped through pixels.
(define (picture-item-xml id i rid)
  (format (string-append "<p:pic><p:nvPicPr><p:cNvPr id=\"~a\" name=\"~a\"~a/>"
                         "<p:cNvPicPr/><p:nvPr/></p:nvPicPr>"
                         "<p:blipFill><a:blip r:embed=\"~a\">~a</a:blip>~a"
                         "<a:stretch><a:fillRect/></a:stretch></p:blipFill>"
                         "<p:spPr>~a<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>~a</p:spPr>"
                         "</p:pic>")
          id (xml-escape (or (it:picture-tag i) (format "Picture ~a" id)))
          (if (it:picture-tag i)
              (format " descr=\"glide-pptx:~a\"" (xml-escape (it:picture-tag i))) "")
          rid
          ;; A washed-out picture is an alphaModFix on the blip, not a fill.
          (let ([o (it:picture-opacity i)])
            (if (< o 0.999) (format "<a:alphaModFix amt=\"~a\"/>" (pct o)) ""))
          (let ([c (it:picture-crop i)])
            (if (and c (ormap (lambda (v) (> (abs v) 1e-9)) c))
                (format "<a:srcRect l=\"~a\" t=\"~a\" r=\"~a\" b=\"~a\"/>"
                        (pct (first c)) (pct (second c)) (pct (third c)) (pct (fourth c)))
                ""))
          (xfrm-xml (it:picture-x i) (it:picture-y i) (it:picture-w i) (it:picture-h i)
                    (it:picture-rot i) (it:picture-flip-h? i) (it:picture-flip-v? i))
          (if (it:picture-pen i) (line-xml (it:picture-pen i)) "")))

;; ------------------------------------------------------------------ images

(define (image-item-xml id i rid)
  (format (string-append "<p:pic><p:nvPicPr><p:cNvPr id=\"~a\" name=\"Picture ~a\"/>"
                         "<p:cNvPicPr/><p:nvPr/></p:nvPicPr>"
                         "<p:blipFill><a:blip r:embed=\"~a\"/>"
                         "<a:stretch><a:fillRect/></a:stretch></p:blipFill>"
                         "<p:spPr>~a<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom></p:spPr>"
                         "</p:pic>")
          id id rid
          (xfrm-xml (it:image-x i) (it:image-y i) (it:image-w i) (it:image-h i)
                    (it:image-rot i))))

;; ------------------------------------------------------------------- slide

;; Returns (values slide-xml image-parts) where each image part is
;; (list part-name relationship-id bytes-writer).
(define (slide-xml page index)
  (define images '())
  (define shapes
    (for/list ([i (in-list (display-page-items page))] [n (in-naturals 2)])
      (cond
        [(it:rect? i) (rect-item-xml n i)]
        [(it:ellipse? i) (ellipse-item-xml n i)]
        [(it:path? i) (path-item-xml n i)]
        [(it:text? i) (text-item-xml n i)]
        [(it:image? i)
         (define k (add1 (length images)))
         (define rid (format "rId~a" (+ 1 k)))
         (define name (format "ppt/media/image~a-~a.png" index k))
         (set! images (cons (list name rid i) images))
         (image-item-xml n i rid)]
        [(it:preset? i) (preset-item-xml n i)]
        [(it:textbox? i) (textbox-item-xml n i)]
        [(it:shape-path? i) (shape-path-item-xml n i)]
        [(it:picture? i)
         (define k (add1 (length images)))
         (define rid (format "rId~a" (+ 1 k)))
         (define src (it:picture-src i))
         (define ext (let ([e (bytes->string/utf-8
                               (or (path-get-extension (if (path? src) src (string->path src)))
                                   #".png"))])
                       (if (member (string-downcase e) '(".png" ".jpg" ".jpeg" ".gif"))
                           (string-downcase e) ".png")))
         (define name (format "ppt/media/pic~a-~a~a" index k ext))
         (set! images (cons (list name rid i) images))
         (picture-item-xml n i rid)]
        [else ""])))
  (values
   (format (string-append xml-decl
                          "<p:sld xmlns:a=\"~a\" xmlns:r=\"~a\" xmlns:p=\"~a\">"
                          "<p:cSld>~a<p:spTree>"
                          "<p:nvGrpSpPr><p:cNvPr id=\"1\" name=\"\"/><p:cNvGrpSpPr/>"
                          "<p:nvPr/></p:nvGrpSpPr>"
                          "<p:grpSpPr><a:xfrm><a:off x=\"0\" y=\"0\"/>"
                          "<a:ext cx=\"0\" cy=\"0\"/><a:chOff x=\"0\" y=\"0\"/>"
                          "<a:chExt cx=\"0\" cy=\"0\"/></a:xfrm></p:grpSpPr>"
                          "~a</p:spTree></p:cSld>"
                          "<p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sld>")
           NS-A NS-R NS-P
           ;; A page-wide fill is the slide's background, which is where
           ;; PowerPoint keeps it -- not an extra shape on the slide.
           (let ([bg (display-page-background page)])
             (if bg (format "<p:bg><p:bgPr>~a<a:effectLst/></p:bgPr></p:bg>" (fill-xml bg)) ""))
           (apply string-append shapes))
   (reverse images)))

;; ---------------------------------------------------------------- package

(define xml-decl "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")
(define NS-A "http://schemas.openxmlformats.org/drawingml/2006/main")
(define NS-R "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
(define NS-P "http://schemas.openxmlformats.org/presentationml/2006/main")
(define REL "http://schemas.openxmlformats.org/officeDocument/2006/relationships")

(define (content-types slide-count image-names)
  (format (string-append xml-decl
                         "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
                         "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
                         "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
                         "<Default Extension=\"png\" ContentType=\"image/png\"/>"
                         "<Default Extension=\"jpg\" ContentType=\"image/jpeg\"/>"
                         "<Default Extension=\"jpeg\" ContentType=\"image/jpeg\"/>"
                         "<Default Extension=\"gif\" ContentType=\"image/gif\"/>"
                         "<Override PartName=\"/ppt/presentation.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.presentationml.presentation.main+xml\"/>"
                         "<Override PartName=\"/ppt/slideMasters/slideMaster1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.presentationml.slideMaster+xml\"/>"
                         "<Override PartName=\"/ppt/slideLayouts/slideLayout1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.presentationml.slideLayout+xml\"/>"
                         "<Override PartName=\"/ppt/theme/theme1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.theme+xml\"/>"
                         "~a</Types>")
          (apply string-append
                 (for/list ([i (in-range 1 (add1 slide-count))])
                   (format "<Override PartName=\"/ppt/slides/slide~a.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.presentationml.slide+xml\"/>" i)))))

(define root-rels
  (format (string-append xml-decl
                         "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                         "<Relationship Id=\"rId1\" Type=\"~a/officeDocument\" Target=\"ppt/presentation.xml\"/>"
                         "</Relationships>")
          REL))

(define (presentation-xml n width height)
  (format (string-append xml-decl
                         "<p:presentation xmlns:a=\"~a\" xmlns:r=\"~a\" xmlns:p=\"~a\">"
                         "<p:sldMasterIdLst><p:sldMasterId id=\"2147483648\" r:id=\"rId1\"/></p:sldMasterIdLst>"
                         "<p:sldIdLst>~a</p:sldIdLst>"
                         "<p:sldSz cx=\"~a\" cy=\"~a\"/><p:notesSz cx=\"~a\" cy=\"~a\"/>"
                         "</p:presentation>")
          NS-A NS-R NS-P
          (apply string-append
                 (for/list ([i (in-range 1 (add1 n))])
                   (format "<p:sldId id=\"~a\" r:id=\"rId~a\"/>" (+ 255 i) (+ 1 i))))
          (emu width) (emu height) (emu height) (emu width)))

(define (presentation-rels n)
  (format (string-append xml-decl
                         "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                         "<Relationship Id=\"rId1\" Type=\"~a/slideMaster\" Target=\"slideMasters/slideMaster1.xml\"/>"
                         "~a"
                         "<Relationship Id=\"rId~a\" Type=\"~a/theme\" Target=\"theme/theme1.xml\"/>"
                         "</Relationships>")
          REL
          (apply string-append
                 (for/list ([i (in-range 1 (add1 n))])
                   (format "<Relationship Id=\"rId~a\" Type=\"~a/slide\" Target=\"slides/slide~a.xml\"/>"
                           (+ 1 i) REL i)))
          (+ n 2) REL))

(define blank-sp-tree
  (string-append "<p:cSld><p:spTree>"
                 "<p:nvGrpSpPr><p:cNvPr id=\"1\" name=\"\"/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr>"
                 "<p:grpSpPr/></p:spTree></p:cSld>"))

(define master-xml
  (format (string-append xml-decl
                         "<p:sldMaster xmlns:a=\"~a\" xmlns:r=\"~a\" xmlns:p=\"~a\">~a"
                         "<p:clrMap bg1=\"lt1\" tx1=\"dk1\" bg2=\"lt2\" tx2=\"dk2\""
                         " accent1=\"accent1\" accent2=\"accent2\" accent3=\"accent3\""
                         " accent4=\"accent4\" accent5=\"accent5\" accent6=\"accent6\""
                         " hlink=\"hlink\" folHlink=\"folHlink\"/>"
                         "<p:sldLayoutIdLst><p:sldLayoutId id=\"2147483649\" r:id=\"rId1\"/>"
                         "</p:sldLayoutIdLst></p:sldMaster>")
          NS-A NS-R NS-P blank-sp-tree))

(define master-rels
  (format (string-append xml-decl
                         "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                         "<Relationship Id=\"rId1\" Type=\"~a/slideLayout\" Target=\"../slideLayouts/slideLayout1.xml\"/>"
                         "<Relationship Id=\"rId2\" Type=\"~a/theme\" Target=\"../theme/theme1.xml\"/>"
                         "</Relationships>")
          REL REL))

(define layout-xml
  (format (string-append xml-decl
                         "<p:sldLayout xmlns:a=\"~a\" xmlns:r=\"~a\" xmlns:p=\"~a\""
                         " type=\"blank\" preserve=\"1\">~a"
                         "<p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sldLayout>")
          NS-A NS-R NS-P blank-sp-tree))

(define layout-rels
  (format (string-append xml-decl
                         "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                         "<Relationship Id=\"rId1\" Type=\"~a/slideMaster\" Target=\"../slideMasters/slideMaster1.xml\"/>"
                         "</Relationships>")
          REL))

(define (slide-rels image-parts)
  (format (string-append xml-decl
                         "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                         "<Relationship Id=\"rId1\" Type=\"~a/slideLayout\" Target=\"../slideLayouts/slideLayout1.xml\"/>"
                         "~a</Relationships>")
          REL
          (apply string-append
                 (for/list ([p (in-list image-parts)])
                   (format "<Relationship Id=\"~a\" Type=\"~a/image\" Target=\"../media/~a\"/>"
                           (second p) REL (file-name-from-path (first p)))))))

;; A theme is mandatory. This one is minimal but complete enough to satisfy
;; PowerPoint's schema, and nothing on our slides refers to it.
(define (font-entry tag face) (format "<a:~a typeface=\"~a\"/>" tag face))
(define theme-xml
  (let* ([scheme-colors
          (string-append
           "<a:dk1><a:sysClr val=\"windowText\" lastClr=\"000000\"/></a:dk1>"
           "<a:lt1><a:sysClr val=\"window\" lastClr=\"FFFFFF\"/></a:lt1>"
           "<a:dk2><a:srgbClr val=\"44546A\"/></a:dk2><a:lt2><a:srgbClr val=\"E7E6E6\"/></a:lt2>"
           "<a:accent1><a:srgbClr val=\"4472C4\"/></a:accent1>"
           "<a:accent2><a:srgbClr val=\"ED7D31\"/></a:accent2>"
           "<a:accent3><a:srgbClr val=\"A5A5A5\"/></a:accent3>"
           "<a:accent4><a:srgbClr val=\"FFC000\"/></a:accent4>"
           "<a:accent5><a:srgbClr val=\"5B9BD5\"/></a:accent5>"
           "<a:accent6><a:srgbClr val=\"70AD47\"/></a:accent6>"
           "<a:hlink><a:srgbClr val=\"0563C1\"/></a:hlink>"
           "<a:folHlink><a:srgbClr val=\"954F72\"/></a:folHlink>")]
         [fonts (string-append
                 "<a:majorFont>" (font-entry 'latin "Calibri Light")
                 (font-entry 'ea "") (font-entry 'cs "") "</a:majorFont>"
                 "<a:minorFont>" (font-entry 'latin "Calibri")
                 (font-entry 'ea "") (font-entry 'cs "") "</a:minorFont>")]
         [fill "<a:solidFill><a:schemeClr val=\"phClr\"/></a:solidFill>"]
         [ln (string-append "<a:ln w=\"6350\" cap=\"flat\" cmpd=\"sng\" algn=\"ctr\">"
                            fill "<a:prstDash val=\"solid\"/></a:ln>")])
    (format (string-append xml-decl
                           "<a:theme xmlns:a=\"~a\" name=\"glide-pptx\"><a:themeElements>"
                           "<a:clrScheme name=\"glide-pptx\">~a</a:clrScheme>"
                           "<a:fontScheme name=\"glide-pptx\">~a</a:fontScheme>"
                           "<a:fmtScheme name=\"glide-pptx\">"
                           "<a:fillStyleLst>~a~a~a</a:fillStyleLst>"
                           "<a:lnStyleLst>~a~a~a</a:lnStyleLst>"
                           "<a:effectStyleLst>~a~a~a</a:effectStyleLst>"
                           "<a:bgFillStyleLst>~a~a~a</a:bgFillStyleLst>"
                           "</a:fmtScheme></a:themeElements></a:theme>")
            NS-A scheme-colors fonts
            fill fill fill ln ln ln
            "<a:effectStyle><a:effectLst/></a:effectStyle>"
            "<a:effectStyle><a:effectLst/></a:effectStyle>"
            "<a:effectStyle><a:effectLst/></a:effectStyle>"
            fill fill fill)))

;; ------------------------------------------------------------------ writing

;; Writes `pages` as a .pptx at `path`. All pages take the size of the first.
(define (display-pages->pptx pages path)
  (when (null? pages) (error 'display-pages->pptx "no pages"))
  (define width (display-page-width (first pages)))
  (define height (display-page-height (first pages)))
  (define dir (make-temporary-file "pptx-out~a" 'directory))
  (define (put! name content)
    (define full (build-path dir (string->path name)))
    (make-directory* (path-only full))
    (if (bytes? content)
        (call-with-output-file full #:exists 'replace (lambda (o) (write-bytes content o)))
        (call-with-output-file full #:exists 'replace (lambda (o) (write-string content o)))))
  (define all-images '())
  (define names '())
  (define (add! name content) (put! name content) (set! names (cons name names)))
  (for ([page (in-list pages)] [i (in-naturals 1)])
    (define-values (xml images) (slide-xml page i))
    (add! (format "ppt/slides/slide~a.xml" i) xml)
    (add! (format "ppt/slides/_rels/slide~a.xml.rels" i) (slide-rels images))
    (for ([im (in-list images)])
      (add! (first im) (item-bytes (third im)))
      (set! all-images (cons (first im) all-images))))
  (add! "[Content_Types].xml" (content-types (length pages) all-images))
  (add! "_rels/.rels" root-rels)
  (add! "ppt/presentation.xml" (presentation-xml (length pages) width height))
  (add! "ppt/_rels/presentation.xml.rels" (presentation-rels (length pages)))
  (add! "ppt/slideMasters/slideMaster1.xml" master-xml)
  (add! "ppt/slideMasters/_rels/slideMaster1.xml.rels" master-rels)
  (add! "ppt/slideLayouts/slideLayout1.xml" layout-xml)
  (add! "ppt/slideLayouts/_rels/slideLayout1.xml.rels" layout-rels)
  (add! "ppt/theme/theme1.xml" theme-xml)
  (define target (path->complete-path path))
  (when (file-exists? target) (delete-file target))
  (parameterize ([current-directory dir])
    (apply zip target (map string->path (reverse names))))
  (delete-directory/files dir #:must-exist? #f)
  target)

;; A picture item names a file, so its original encoding is kept; a bitmap item
;; only has pixels, and has to be encoded.
(define (item-bytes i)
  (cond
    [(it:picture? i)
     (define src (it:picture-src i))
     (if (file-exists? src)
         (file->bytes src)
         (begin (warn! "image ~a is missing; a blank stands in" src)
                (blank-png)))]
    [else (png-bytes i)]))

(define (blank-png)
  (define bm (make-bitmap 1 1))
  (define o (open-output-bytes))
  (send bm save-file o 'png)
  (get-output-bytes o))

;; The recorded bitmap is raw ARGB rows; round-trip it through a bitmap% to get
;; a PNG, which is the only raster format worth putting in a package.
(define (png-bytes i)
  (define w (it:image-src-w i)) (define h (it:image-src-h i))
  (define bm (make-bitmap w h))
  (send bm set-argb-pixels 0 0 w h (it:image-argb i))
  (define o (open-output-bytes))
  (send bm save-file o 'png)
  (get-output-bytes o))
