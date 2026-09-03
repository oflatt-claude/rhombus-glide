#lang racket/base
;; The parent process: keep a Pict program and a presentation in step.
;;
;;   program saved  ->  regenerate the deck, tell the app to show it
;;   deck saved     ->  merge the geometry back into the program
;;
;; Change detection is by content hash rather than by filesystem event. That
;; costs a poll and buys three things: it works for a Keynote `.key`, which is a
;; directory rather than a file; it collapses the burst of writes an editor makes
;; when saving; and it is the loop guard, since a file we just wrote ourselves
;; hashes to what we expect and so does not look like someone else's edit.
(require racket/list racket/string racket/format racket/file racket/path
         racket/system racket/port file/sha1
         "export.rkt" "sync.rkt")
(provide (struct-out app-adapter) adapters adapter-named
         watch-loop watch-once program-picts
         current-watch-log)

(define current-watch-log (make-parameter (lambda (fmt . args)
                                            (apply printf fmt args)
                                            (flush-output))))
(define (log! fmt . args) (apply (current-watch-log) fmt args))

;; -------------------------------------------------------------- adapters

;; `document` is the path the user actually edits, given the deck we generate:
;; the same file for an app whose native format is .pptx, and a .key bundle for
;; Keynote. `harvest!` turns that document back into a .pptx we can read, and
;; `reload!` makes the app show a deck we just regenerated.
(struct app-adapter (name document harvest! reload!) #:transparent)

(define (osascript . lines)
  (define exe (find-executable-path "osascript"))
  (cond
    [(not exe) (log! "  ! osascript not found; this needs macOS\n") #f]
    [else
     (define out (open-output-string))
     (define code
       (parameterize ([current-output-port out] [current-error-port out])
         (apply system*/exit-code exe (append* (for/list ([l (in-list lines)])
                                                 (list "-e" l))))))
     (unless (zero? code) (log! "  ! osascript: ~a\n" (string-trim (get-output-string out))))
     (zero? code)]))

;; Nothing outside the files. Used by the tests, and by anyone who would rather
;; drive their editor themselves.
(define none-adapter
  (app-adapter 'none (lambda (pptx) pptx) (lambda (doc pptx) #t) (lambda (pptx) #t)))

;; PowerPoint edits .pptx natively, so the document *is* the deck and there is
;; nothing to harvest -- the easiest case, and the one to prefer if the choice
;; is open. Untested here: this box has no macOS.
(define powerpoint-adapter
  (app-adapter
   'powerpoint
   (lambda (pptx) pptx)
   (lambda (doc pptx) #t)
   (lambda (pptx)
     (define p (path->string (path->complete-path pptx)))
     (osascript "tell application \"Microsoft PowerPoint\""
                (format "  set target to POSIX file \"~a\"" p)
                "  repeat with d in presentations"
                (format "    if (full name of d) is \"~a\" then close d saving no" p)
                "  end repeat"
                "  open target"
                "  activate"
                "end tell"))))

;; Keynote's own format is a .key bundle and it never saves .pptx, so the deck
;; has to be exported out of it before a merge can read it, and a regenerated
;; deck has to be re-imported. Keynote has no reload, so the document is closed
;; and reopened, which loses the current slide and selection. Untested here.
(define keynote-adapter
  (app-adapter
   'keynote
   ;; The user edits a .key beside the deck we generate.
   (lambda (pptx) (path-replace-extension pptx ".key"))
   (lambda (doc pptx)
     (define d (path->string (path->complete-path doc)))
     (define out (path->string (path->complete-path pptx)))
     (osascript "tell application \"Keynote\""
                (format "  set d to (open POSIX file \"~a\")" d)
                (format "  export d to POSIX file \"~a\" as Microsoft PowerPoint" out)
                "end tell"))
   (lambda (pptx)
     (define p (path->string (path->complete-path pptx)))
     (osascript "tell application \"Keynote\""
                "  repeat with d in documents"
                "    close d saving no"
                "  end repeat"
                (format "  open POSIX file \"~a\"" p)
                "  activate"
                "end tell"))))

;; Testable here: LibreOffice has no reload either, so it is opened afresh.
(define libreoffice-adapter
  (app-adapter
   'libreoffice
   (lambda (pptx) pptx)
   (lambda (doc pptx) #t)
   (lambda (pptx)
     (define exe (or (find-executable-path "soffice") (find-executable-path "libreoffice")))
     (and exe (begin (process/ports (open-output-nowhere) #f (open-output-nowhere)
                                    (format "~a --norestore ~a" exe
                                            (path->string (path->complete-path pptx))))
                     #t)))))

(define adapters
  (hash 'none none-adapter 'powerpoint powerpoint-adapter
        'keynote keynote-adapter 'libreoffice libreoffice-adapter))

(define (adapter-named name)
  (hash-ref adapters name
            (lambda () (error 'watch "no adapter named ~a; try one of ~a"
                              name (sort (map symbol->string (hash-keys adapters))
                                         string<?)))))

;; ----------------------------------------------------------------- hashing

;; A .key is a directory, so a bundle is summarized by what it contains rather
;; than read whole.
(define (content-hash path)
  (cond
    [(not (or (file-exists? path) (directory-exists? path))) #f]
    [(directory-exists? path)
     (sha1 (open-input-string
            (string-join
             (for/list ([p (in-list (sort (map path->string (all-files path)) string<?))])
               (define f (string->path p))
               (format "~a:~a:~a" p (file-size f) (file-or-directory-modify-seconds f)))
             "\n")))]
    [else (call-with-input-file path sha1)]))

(define (all-files dir)
  (append*
   (for/list ([p (in-list (directory-list dir #:build? #t))])
     (cond [(directory-exists? p) (all-files p)]
           [else (list p)]))))

;; Waits for a path to stop changing, so one save is one event.
(define (settle path #:quiet [quiet 0.25] #:limit [limit 5.0])
  (let loop ([h (content-hash path)] [waited 0.0])
    (sleep quiet)
    (define h2 (content-hash path))
    (cond
      [(equal? h h2) h2]
      [(>= waited limit) h2]
      [else (loop h2 (+ waited quiet))])))

;; ------------------------------------------------------------------- steps

;; program -> deck, then show it.
(define (regenerate! program pptx adapter #:width [w #f] #:height [h #f])
  (log! "program changed -> regenerating ~a\n" (file-name-from-path pptx))
  (define warnings (box '()))
  (with-handlers ([exn:fail? (lambda (e)
                               (log! "  ! the program did not run: ~a\n"
                                     (first (string-split (exn-message e) "\n")))
                               #f)])
    (define picts (program-picts program))
    (parameterize ([current-export-warnings warnings])
      (picts->pptx picts pptx #:width w #:height h))
    (for ([m (in-list (remove-duplicates (reverse (unbox warnings))))])
      (log! "  note: ~a\n" m))
    ((app-adapter-reload! adapter) pptx)
    (log! "  ~a slides written\n" (length picts))
    #t))

(define (program-picts program) (load-program-picts program))

;; deck -> program.
(define (merge-back! program pptx document adapter workdir)
  (log! "deck changed -> merging into ~a\n" (file-name-from-path program))
  (with-handlers ([exn:fail? (lambda (e)
                               (log! "  ! merge failed: ~a\n"
                                     (first (string-split (exn-message e) "\n")))
                               #f)])
    ;; For an app that does not save .pptx, get one out of it first.
    (unless (equal? (path->string document) (path->string pptx))
      ((app-adapter-harvest! adapter) document pptx))
    (define r (sync-once program pptx #:workdir workdir))
    (define text (format-sync-report r))
    (for ([l (in-list (string-split text "\n"))]) (log! "~a\n" l))
    #t))

;; ------------------------------------------------------------------- loop

;; One pass, for tests and for a single shot from the command line.
(define (watch-once program pptx #:adapter [adapter none-adapter]
                    #:width [w #f] #:height [h #f] #:workdir [workdir #f])
  (regenerate! program pptx adapter #:width w #:height h))

;; Polls until interrupted. Only one side is acted on per tick, and the program
;; wins, so a burst of edits on both cannot interleave into a half-merge.
(define (watch-loop program pptx
                    #:adapter [adapter none-adapter]
                    #:width [w #f] #:height [h #f]
                    #:workdir [workdir #f]
                    #:interval [interval 0.4]
                    #:ticks [ticks #f])
  (define document ((app-adapter-document adapter) pptx))
  (log! "watching\n  program ~a\n  deck    ~a\n  app     ~a\n"
        program document (app-adapter-name adapter))
  ;; Start from the program, so the deck exists and both sides agree.
  (regenerate! program pptx adapter #:width w #:height h)
  (sync-once program pptx #:workdir workdir)
  (let loop ([prog-hash (content-hash program)]
             [doc-hash (content-hash document)]
             [n 0])
    (cond
      [(and ticks (>= n ticks)) (log! "done\n")]
      [else
       (sleep interval)
       (define ph (content-hash program))
       (define dh (content-hash document))
       (cond
         [(and ph (not (equal? ph prog-hash)))
          (settle program)
          (regenerate! program pptx adapter #:width w #:height h)
          ;; Re-read both, since we just wrote the deck ourselves.
          (loop (content-hash program) (content-hash document) (add1 n))]
         [(and dh (not (equal? dh doc-hash)))
          (settle document)
          (merge-back! program pptx document adapter workdir)
          (loop (content-hash program) (content-hash document) (add1 n))]
         [else (loop prog-hash doc-hash (add1 n))])])))
