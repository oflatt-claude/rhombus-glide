#lang racket/base
;; The editor doing the editing.
;;
;; Every other test here simulates an editor by writing the XML I imagine one
;; writes -- a shape with a name and with our own tag on it, an attribute
;; changed in place. A real editor does not write what I imagine. LibreOffice
;; names a shape it draws `name=""`, and nameless it had no key: the merge could
;; not name it, find it, or write it, so it refused, and since a save lands
;; whole or not at all it took every other edit with it. Nothing here saw that,
;; because nothing here had ever let the editor be the editor.
;;
;; So this drives a real LibreOffice over the same interface the adapter uses:
;; draw a rectangle, drag a shape, retype a line, recolour a fill, then save.
;; Each one has to merge, land in the program, and settle.
;;
;; Needs LibreOffice, python-uno and somewhere to draw. Without them it says so
;; and passes.
(require rackunit/log)
(require rackunit racket/file racket/path racket/string racket/system racket/port
         racket/runtime-path
         glide-pptx/export glide-pptx/sync glide-pptx/watch)

(define-runtime-path driver "lo-edit.py")

(define PORT "2143")

(define (uno-ready?)
  (define py (or (for/or ([p (in-list '("/Applications/LibreOffice.app/Contents/MacOS/python"
                                        "/usr/lib/libreoffice/program/python"))])
                   (and (file-exists? p) p))
                 (let ([p (find-executable-path "python3")]) (and p (path->string p)))))
  (and py
       (zero? (parameterize ([current-output-port (open-output-nowhere)]
                             [current-error-port (open-output-nowhere)])
                (system*/exit-code py "-c" "import uno, unohelper")))
       py))

(define (drive! py what deck)
  (parameterize ([current-output-port (open-output-nowhere)]
                 [current-error-port (open-output-nowhere)])
    (system*/exit-code py (path->string driver) what PORT (path->string deck))))

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-lo-edits"))

(define program-text
  (string-join
   (list "#lang rhombus/and_meta"
         "import:"
         "  lib(\"glide-pptx/runtime.rhm\") open"
         "export:"
         "  all_slides"
         "def slide_1 = slide_canvas("
         "  ~width: 720.0, ~height: 540.0, ~background: hex(\"FFFFFF\"),"
         "  at(40.0, 40.0, ~tag: \"Box\","
         "     shape_pict(~width: 120.0, ~height: 80.0, ~fill: hex(\"4472C4\"))),"
         "  at(240.0, 200.0, ~tag: \"Words\","
         "     textbox(~width: 300.0, ~height: 60.0,"
         "             para(run(\"hello there\", ~size: 24.0))))"
         ")"
         "def all_slides = [slide_1]"
         "")
   "\n"))

(define py (and (soffice-exe) (getenv "DISPLAY") (uno-ready?)))

(cond
  [(not py)
   (printf "no LibreOffice to drive (needs soffice, python-uno and a display); skipped\n")]
  [else
   (delete-directory/files work #:must-exist? #f)
   (make-directory* work)
   (define program (build-path work "p.rhm"))
   (define deck (build-path work "p.pptx"))
   (display-to-file program-text program #:exists 'replace)
   (picts->pptx (load-program-picts program) deck)
   (void (sync-once program deck #:workdir (build-path work "w")))

   ;; Opened in the editor, and waited for: it has to be holding the deck
   ;; before it can be asked to change it.
   (void ((app-adapter-reload! (adapter-named 'libreoffice)) deck))
   (define open?
     (let loop ([n 0])
       (cond [(zero? (drive! py "open" deck)) #t]
             [(> n 40) #f]
             [else (sleep 1) (loop (add1 n))])))
   (check-true open? "LibreOffice opened the deck")

   (when open?
     (define (merge!) (sync-once program deck #:workdir (build-path work "w") #:atomic? #t))
     (define (edit! what)
       (check-equal? (drive! py what deck) 0 (format "LibreOffice was asked to ~a" what))
       (merge!))

     ;; Saved with nothing changed. Whatever the editor rewrites on its way
     ;; through is not an edit, and none of it may reach the program.
     (define r0 (edit! "touch"))
     (check-equal? (sync-report-applied r0) '()
                   "a save with nothing changed writes nothing")
     (check-equal? (sync-report-skipped r0) '()
                   "and refuses nothing")

     ;; A shape it drew itself, which is the one it does not name.
     (define before (file->string program))
     (define r1 (edit! "draw"))
     (check-equal? (sync-report-skipped r1) '() "the drawn shape was not refused")
     (check-equal? (map sync-action-kind (sync-report-applied r1)) '(added)
                   "it was added")
     (check-false (equal? before (file->string program)) "and the program says so")
     (check-true (pair? (program-picts program)) "which still reads")

     (define r2 (edit! "move"))
     (check-equal? (sync-report-skipped r2) '() "a drag was not refused")
     (check-true (and (memq 'moved (map sync-action-kind (sync-report-applied r2))) #t)
                 "and came through as a move")

     (define r3 (edit! "retext"))
     (check-equal? (sync-report-skipped r3) '() "a retyping was not refused")
     (check-regexp-match #rx"edited in libreoffice" (file->string program)
                         "and the words are in the program")

     (define r4 (edit! "recolor"))
     (check-equal? (sync-report-skipped r4) '() "a recolour was not refused")
     (check-regexp-match #rx"70AD47" (file->string program)
                         "and the colour is in the program")

     ;; And after all of it, the two agree.
     (picts->pptx (load-program-picts program) deck)
     (define settled (sync-once program deck #:workdir (build-path work "w") #:atomic? #t))
     (check-equal? (filter (lambda (a) (not (eq? 'noted (sync-action-kind a))))
                           (sync-report-actions settled))
                   '()
                   "and the session settles"))
   (printf "libreoffice edit tests done\n")])

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
