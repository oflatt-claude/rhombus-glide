#lang racket/base
;; Unit tests for the pieces that are pure functions of the file format.
(require rackunit racket/list racket/class racket/draw
         glide-pptx/units glide-pptx/xml-util glide-pptx/ir
         glide-pptx/theme glide-pptx/geometry glide-pptx/opc)

;; ------------------------------------------------------------------- units

(check-= (emu->pt 914400) 72.0 1e-9 "an inch is 72 points")
(check-= (emu->pt 12700) 1.0 1e-9 "12700 EMU is one point")
(check-= (hundredths->pt 4400) 44.0 1e-9 "font sizes are hundredths of a point")
(check-= (angle->degrees 1200000) 20.0 1e-9 "rotations are 60000ths of a degree")
(check-= (percent->fraction 20000) 0.2 1e-9 "percentages are 1000ths of a percent")
(check-= (string->percent "50%") 0.5 1e-9 "a literal percent sign is accepted too")
(check-equal? (string->bool "1") #t)
(check-equal? (string->bool "false") #f)
(check-equal? (string->bool #f 'fallback) 'fallback "an absent attribute takes the default")

;; --------------------------------------------------------------- xml-util

(define slide-xml
  '(p:sld ((xmlns:a "urn:a"))
          (p:cSld ()
                  (p:spTree ()
                            (p:sp () (p:spPr () (a:xfrm ((rot "1200000"))
                                                        (a:off ((x "914400") (y "0")))
                                                        (a:ext ((cx "914400") (cy "457200"))))))
                            (p:sp () (p:txBody () (a:p () (a:r () (a:t () "hello"))
                                                       (a:br ())
                                                       (a:r () (a:t () "world")))))))))

(check-equal? (local-name 'a:off) 'off "the prefix is ignored")
(check-equal? (local-name 'off) 'off "an unprefixed name is its own local name")
(define tree (xpath slide-xml 'cSld 'spTree))
(check-equal? (length (children tree 'sp)) 2)
(check-equal? (attr (xpath (first (children tree 'sp)) 'spPr 'xfrm 'off) 'x) "914400")
(check-equal? (all-text (xpath (second (children tree 'sp)) 'txBody))
              "hello\nworld"
              "<a:br/> reads as a newline")

;; An element with both an unprefixed and a namespaced attribute of the same
;; local name: the relationship is the namespaced one.
(define sld-id '(p:sldId ((id "256") (r:id "rId7"))))
(check-equal? (attr sld-id 'id) "256")
(check-equal? (attr-ns sld-id 'id) "rId7" "attr-ns only matches a prefixed name")

;; ---------------------------------------------------- relationship targets

(check-equal? (resolve-part-name "ppt/slides/slide1.xml" "../media/image1.png")
              "ppt/media/image1.png")
(check-equal? (resolve-part-name "ppt/slides/slide1.xml" "../slideLayouts/slideLayout2.xml")
              "ppt/slideLayouts/slideLayout2.xml")
(check-equal? (resolve-part-name "ppt/presentation.xml" "slides/slide1.xml")
              "ppt/slides/slide1.xml")
(check-equal? (resolve-part-name "" "ppt/presentation.xml") "ppt/presentation.xml"
              "the package root has no directory to resolve against")
(check-equal? (resolve-part-name "ppt/presentation.xml" "/ppt/theme/theme1.xml")
              "ppt/theme/theme1.xml"
              "a leading slash means package-absolute")

;; -------------------------------------------------------------- theme colors

(define theme-xml
  '(a:theme ()
            (a:themeElements ()
                             (a:clrScheme ()
                                          (a:dk1 () (a:sysClr ((val "windowText")
                                                               (lastClr "000000"))))
                                          (a:lt1 () (a:sysClr ((val "window")
                                                               (lastClr "FFFFFF"))))
                                          (a:accent1 () (a:srgbClr ((val "4472C4")))))
                             (a:fontScheme ()
                                           (a:majorFont () (a:latin ((typeface "Calibri Light"))))
                                           (a:minorFont () (a:latin ((typeface "Calibri"))))))))

(define th (parse-theme theme-xml))
(define ctx (clr-ctx th (hash 'tx1 'dk1 'bg1 'lt1 'accent1 'accent1) #f))

(check-equal? (resolve-color ctx '(a:srgbClr ((val "FF8000")))) (rgb 255 128 0))
(check-equal? (resolve-color ctx '(a:schemeClr ((val "accent1")))) (rgb 68 114 196)
              "a scheme color resolves through the color map into the theme")
(check-equal? (resolve-color ctx '(a:schemeClr ((val "tx1")))) black
              "tx1 maps to dk1, whose sysClr carries its last known value")
(check-equal? (resolve-color ctx '(a:prstClr ((val "red")))) (rgb 255 0 0))
(check-= (rgba-a (resolve-color ctx '(a:srgbClr ((val "000000")) (a:alpha ((val "40000"))))))
         0.4 1e-6 "alpha is a percentage")

;; A tint lightens toward white and a shade darkens toward black.
(let ([tinted (resolve-color ctx '(a:srgbClr ((val "000000")) (a:tint ((val "50000")))))]
      [shaded (resolve-color ctx '(a:srgbClr ((val "FFFFFF")) (a:shade ((val "50000")))))])
  (check-= (rgba-r tinted) 127.5 0.6)
  (check-= (rgba-r shaded) 127.5 0.6))

(check-equal? (theme-latin-font th 'minor) "Calibri")
(check-equal? (resolve-typeface th "+mj-lt") "Calibri Light"
              "+mj-lt is the theme's major latin font")
(check-equal? (resolve-typeface th "Arial") "Arial")

;; A color transform we do not model must not change the color, rather than error.
(check-equal? (resolve-color ctx '(a:srgbClr ((val "112233")) (a:gamma ())))
              (rgb 17 34 51))

;; ------------------------------------------------------------------ geometry

(check-true (preset-known? "roundRect"))
(check-true (preset-known? "star5"))
(check-true (preset-known? "rightArrow"))
(check-false (preset-known? "notAShapeName"))
(check-false (geometry-closed? "line") "a line cannot be filled")
(check-true (geometry-closed? "rect"))

;; Every registered preset must produce a path inside its own box.
(for ([name (in-list (preset-names))])
  (define p (preset-path name 100.0 60.0 '()))
  (define-values (x y w h) (send p get-bounding-box))
  (check-true (and (>= x -0.51) (>= y -0.51) (<= (+ x w) 100.51) (<= (+ y h) 60.51))
              (format "~a stays within its box: ~a ~a ~a ~a" name x y w h)))

;; An unknown preset falls back to the full rectangle rather than failing.
(let ()
  (define p (preset-path "someShapeWeHaveNotMet" 40.0 20.0 '()))
  (define-values (x y w h) (send p get-bounding-box))
  (check-= w 40.0 1e-6)
  (check-= h 20.0 1e-6))

;; Flipping mirrors the geometry within the same box.
(let ()
  (define plain (preset-path "rightArrow" 100.0 50.0 '()))
  (define flipped (preset-path "rightArrow" 100.0 50.0 '() #:flip-h? #t))
  (define-values (x0 y0 w0 h0) (send plain get-bounding-box))
  (define-values (x1 y1 w1 h1) (send flipped get-bounding-box))
  (check-= w1 w0 1e-6 "a flip does not change the width")
  (check-= h1 h0 1e-6 "a flip does not change the height"))

;; --------------------------------------------------------------------- ir

(check-equal? (rgba-hex (rgb 68 114 196)) "4472C4")
(check-equal? (rgba-hex (rgb 0 0 0)) "000000" "single digits are padded")
(let-values ([(cx cy) (bbox-center (make-bbox 10.0 20.0 100.0 50.0))])
  (check-= cx 60.0 1e-9)
  (check-= cy 45.0 1e-9))
(check-true (text-body-empty? #f))
(check-true (text-body-empty?
             (text-body (list (para (list (trun "" "Calibri" 18.0 #f #f #f #f black 0.0 'none 0.0))
                                    'left 0 0.0 0.0 '(percent . 1.0) 0.0 0.0 no-bullet))
                        'top #f #t 'none default-insets 0.0)))

;; A deck round-trips through `write`/`read`, which is what golden IR relies on.
(let* ([d (deck 960.0 540.0
                (list (slide 1 "One" 960.0 540.0 #f '()
                             (list (shape 2 "Box" (make-bbox 0.0 0.0 10.0 10.0)
                                          (preset-geom "rect" '())
                                          (solid-fill (rgb 1 2 3)) #f #f))))
                "/tmp" "x.pptx")]
       [again (read (open-input-string (format "~s" d)))])
  (check-equal? again d "prefab structs survive a write/read round trip"))

;; ------------------------------------------------------- dc state hygiene

;; pict's `dc` contract rejects a draw procedure that leaves the dc changed, so
;; simply constructing and drawing these is the test. Both cases were real bugs
;; found on a deck with outlined pictures: a strikethrough run set a pen, and an
;; outlined picture set a brush, and neither put it back.
(let ()
  (local-require glide-pptx/runtime pict)
  (define (render p)
    (define bm (make-bitmap (max 1 (inexact->exact (ceiling (pict-width p))))
                            (max 1 (inexact->exact (ceiling (pict-height p))))))
    (draw-pict p (new bitmap-dc% [bitmap bm]) 0 0)
    #t)
  (define struck
    (textbox #:width 120.0 #:height 30.0
             (para* (run* "struck" #:size 14.0 #:strike? #t))))
  (check-true (render struck) "a strikethrough run leaves the dc as it found it")
  (define outlined
    (shape-pict #:width 60.0 #:height 30.0 #:fill (hex "4472C4")
                #:line (make-stroke (hex "202020") #:width 1.0)
                #:body (body* (para* (run* "x" #:size 10.0 #:strike? #t)))))
  (check-true (render (vc-append 4 struck outlined)) "and so does a composition of them")
  (check-true (render (slide-canvas #:width 200.0 #:height 100.0
                                    (at 10.0 10.0 struck)
                                    (at 10.0 50.0 outlined)))
              "and a whole slide"))

(printf "unit tests done\n")
