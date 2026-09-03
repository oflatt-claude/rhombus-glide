#lang racket/base
;; Editing a .pptx the way an editor would, for tests.
;;
;; Racket's own zip and unzip are used rather than the command-line tools, so a
;; test that simulates a drag needs nothing installed beyond Racket.
(require racket/list racket/string racket/file racket/path racket/format
         file/unzip file/zip)
(provide with-unpacked-deck drag-in-deck! deck-part)

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
