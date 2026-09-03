#lang racket/base
;; Editing a .pptx the way an editor would, for tests.
;;
;; Racket's own zip and unzip are used rather than the command-line tools, so a
;; test that simulates a drag needs nothing installed beyond Racket.
(require racket/list racket/string racket/file racket/path racket/format
         file/unzip file/zip)
(provide with-unpacked-deck drag-in-deck! deck-part
         add-shape-to-deck! delete-from-deck! nudge-family-in-deck!)

;; Unpacks `pptx`, calls `proc` with the directory, and repacks whatever is
;; there back over the original.
(define (with-unpacked-deck pptx proc)
  (define dir (make-temporary-file "deck~a" 'directory))
  (call-with-input-file pptx
    (lambda (in) (parameterize ([current-directory dir])
                   (unzip in (make-filesystem-entry-reader #:exists 'replace)))))
  (define result (proc dir))
  (when (file-exists? pptx) (delete-file pptx))
  (parameterize ([current-directory dir])
    (apply zip (path->complete-path pptx) (directory-list)))
  (delete-directory/files dir #:must-exist? #f)
  result)

(define (deck-part pptx name)
  (define dir (make-temporary-file "part~a" 'directory))
  (call-with-input-file pptx
    (lambda (in) (parameterize ([current-directory dir])
                   (unzip in (make-filesystem-entry-reader)))))
  (define text (file->string (build-path dir (string->path name))))
  (delete-directory/files dir #:must-exist? #f)
  text)

;; Moves the shape tagged `tag` on slide `slide` to (x, y) in points, which is
;; what dragging it in PowerPoint or Keynote amounts to. Returns #t when the
;; shape was found.
(define (drag-in-deck! pptx slide tag x y)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     ;; The tag is in the alt text, and the shape is the element containing it.
     (define rx
       (pregexp (format "(?s:<p:(?:sp|pic)>(?:(?!</p:(?:sp|pic)>).)*?descr=\"glide-pptx:~a\"(?:(?!</p:(?:sp|pic)>).)*?</p:(?:sp|pic)>)"
                        (regexp-quote tag))))
     (define m (regexp-match rx d))
     (cond
       [(not m) #f]
       [else
        (define moved
          (regexp-replace #px"<a:off x=\"-?\\d+\" y=\"-?\\d+\"/>" (first m)
                          (format "<a:off x=\"~a\" y=\"~a\"/>"
                                  (inexact->exact (round (* 12700 x)))
                                  (inexact->exact (round (* 12700 y))))))
        (call-with-output-file part #:exists 'replace
          (lambda (o) (write-string (string-replace d (first m) moved) o)))
        #t]))))

;; The regexp that finds one tagged shape, which is how an editor's edits are
;; located in a part: the tag is in the alt text and the shape is the element
;; that contains it.
(define (shape-rx tag)
  (pregexp (format "(?s:<p:(?:sp|pic)>(?:(?!</p:(?:sp|pic)>).)*?descr=\"glide-pptx:~a\"(?:(?!</p:(?:sp|pic)>).)*?</p:(?:sp|pic)>)"
                   (regexp-quote tag))))

;; Adds a rectangle to slide `slide`, last in the tree, which is what drawing
;; one in PowerPoint or Keynote amounts to. Returns the name it was given.
(define (add-shape-to-deck! pptx slide name
                            #:x [x 100.0] #:y [y 100.0]
                            #:width [w 120.0] #:height [h 80.0]
                            #:fill [fill "FF0000"])
  (define (emu v) (inexact->exact (round (* 12700 v))))
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define sp
       (format (string-append
                "<p:sp><p:nvSpPr><p:cNvPr id=\"9001\" name=\"~a\"/>"
                "<p:cNvSpPr/><p:nvPr/></p:nvSpPr>"
                "<p:spPr><a:xfrm><a:off x=\"~a\" y=\"~a\"/><a:ext cx=\"~a\" cy=\"~a\"/></a:xfrm>"
                "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>"
                "<a:solidFill><a:srgbClr val=\"~a\"/></a:solidFill></p:spPr>"
                "<p:txBody><a:bodyPr/><a:lstStyle/><a:p/></p:txBody></p:sp>")
               name (emu x) (emu y) (emu w) (emu h) fill))
     (call-with-output-file part #:exists 'replace
       (lambda (o) (write-string (string-replace d "</p:spTree>"
                                                 (string-append sp "</p:spTree>"))
                                 o)))
     name)))

;; Deletes the shape tagged `tag` from slide `slide`. Returns #t when it was
;; there to delete.
(define (delete-from-deck! pptx slide tag)
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define m (regexp-match (shape-rx tag) d))
     (cond
       [(not m) #f]
       [else
        (call-with-output-file part #:exists 'replace
          (lambda (o) (write-string (string-replace d (first m) "") o)))
        #t]))))

;; Shifts every shape tagged `tag` on slide `slide` by (dx, dy) points, which is
;; what selecting a row of them and dragging amounts to. Returns how many moved.
(define (nudge-family-in-deck! pptx slide tag dx dy)
  (define (emu v) (inexact->exact (round (* 12700 v))))
  (with-unpacked-deck
   pptx
   (lambda (dir)
     (define part (build-path dir "ppt" "slides" (format "slide~a.xml" slide)))
     (define d (file->string part))
     (define moved 0)
     (define out
       (regexp-replace*
        (shape-rx tag) d
        (lambda (whole . _)
          (set! moved (add1 moved))
          (regexp-replace
           #px"<a:off x=\"(-?\\d+)\" y=\"(-?\\d+)\"/>" whole
           (lambda (_m x y)
             (format "<a:off x=\"~a\" y=\"~a\"/>"
                     (+ (string->number x) (emu dx))
                     (+ (string->number y) (emu dy))))))))
     (call-with-output-file part #:exists 'replace
       (lambda (o) (write-string out o)))
     moved)))
