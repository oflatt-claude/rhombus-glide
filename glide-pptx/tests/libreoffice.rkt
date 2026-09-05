#lang racket/base
;; The LibreOffice adapter, against a real LibreOffice.
;;
;; It is the default editor now, and the one thing it has to do that the others
;; get for free is show a deck that has just been regenerated. LibreOffice has
;; no reload on its command line -- opening a file it already has open raises
;; the window it is already showing -- so the adapter asks over UNO instead,
;; and this is the test that the asking works. Without it the editor shows a
;; deck from several edits ago and a save from there writes that back over the
;; program's work.
;;
;; Needs LibreOffice, python3 with UNO, and somewhere to draw. Without any of
;; them it says so and passes: this is the one test here that cannot run on
;; Racket alone.
(require rackunit/log)
(require rackunit racket/file racket/path racket/system racket/port racket/string
         "../watch.rkt" "../export.rkt" "../parse.rkt")

(define (have-uno?)
  (define py (find-executable-path "python3"))
  (and py (zero? (parameterize ([current-output-port (open-output-nowhere)]
                                [current-error-port (open-output-nowhere)])
                   (system*/exit-code py "-c" "import uno, unohelper")))))

(define soffice (or (find-executable-path "soffice") (find-executable-path "libreoffice")))

(define (slides-open path)
  ;; What the editor holds, asked the same way the adapter asks.
  (define py (find-executable-path "python3"))
  (define out (open-output-string))
  (parameterize ([current-output-port out] [current-error-port (open-output-nowhere)])
    (system* py "-c"
             (string-append
              "import sys, uno, unohelper\n"
              "local = uno.getComponentContext()\n"
              "r = local.ServiceManager.createInstanceWithContext("
              "'com.sun.star.bridge.UnoUrlResolver', local)\n"
              "ctx = r.resolve('uno:socket,host=localhost,port=2143;urp;"
              "StarOffice.ComponentContext')\n"
              "d = ctx.ServiceManager.createInstanceWithContext("
              "'com.sun.star.frame.Desktop', ctx)\n"
              "url = unohelper.systemPathToFileUrl(sys.argv[1])\n"
              "e = d.getComponents().createEnumeration()\n"
              "while e.hasMoreElements():\n"
              "    c = e.nextElement()\n"
              "    try:\n"
              "        if c.getURL() == url:\n"
              "            print(c.getDrawPages().getCount()); break\n"
              "    except Exception: pass\n")
             (path->string path)))
  (string->number (string-trim (get-output-string out))))

(define (wait-for what [tries 40])
  (let loop ([n 0])
    (cond [(what) #t]
          [(>= n tries) #f]
          [else (sleep 1) (loop (add1 n))])))

(cond
  [(not (and soffice (have-uno?) (getenv "DISPLAY")))
   (printf "no LibreOffice to drive (needs soffice, python3-uno and a display); skipped\n")]
  [else
   (define dir (build-path (find-system-path 'temp-dir) "glide-pptx-libreoffice"))
   (delete-directory/files dir #:must-exist? #f)
   (make-directory* dir)
   (define deck (build-path dir "deck.pptx"))
   (define here (collection-file-path "tests" "glide-pptx"))
   (define two (build-path here "decks" "03-shapes.pptx"))
   (define three (build-path here "decks" "05-realistic.pptx"))
   (copy-file two deck #t)
   (define lo (adapter-named 'libreoffice))

   ;; Opened: the adapter launches it, since nothing has it yet.
   (check-true (and ((app-adapter-reload! lo) deck) #t) "LibreOffice was started")
   (check-true (wait-for (lambda () (equal? 2 (slides-open deck))))
               "and it opened the deck")

   ;; Regenerated underneath it, which is what a saved program does.
   (copy-file three deck #t)
   (check-true (and ((app-adapter-reload! lo) deck) #t) "the adapter was asked to reload")
   (check-true (wait-for (lambda () (equal? 3 (slides-open deck))))
               "and the editor shows the deck as it is now, not as it was")

   ;; And it is still the one document: a reload that opened a second window
   ;; would leave the person editing whichever they clicked.
   (check-equal? (slides-open deck) 3 "one document, holding the new deck")
   (printf "libreoffice tests done\n")])

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
