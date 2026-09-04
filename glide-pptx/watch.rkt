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
(provide (struct-out app-adapter) adapters adapter-named scratch-dir-of
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
;; `open?` says whether the editor still has the deck open, so closing it can end
;; the session and clear the scratch away. An adapter that cannot tell says #t
;; and the session runs until it is interrupted.
(struct app-adapter (name document harvest! reload! open?) #:transparent)

;; An adapter that cannot tell whether its editor is still open says #t, and the
;; session then runs until it is interrupted.
(define (always-open) #t)

;; Asked through System Events, which reports on a running process without
;; starting one -- `application "Keynote" is running` can launch it.
(define (process-running? name)
  (define exe (find-executable-path "osascript"))
  (and exe
       (let ([out (open-output-string)])
         (define code
           (parameterize ([current-output-port out] [current-error-port out])
             (system*/exit-code
              exe "-e"
              (format "tell application \"System Events\" to (name of processes) contains \"~a\""
                      name))))
         (and (zero? code)
              (regexp-match? #rx"true" (get-output-string out))))))

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
  (app-adapter 'none (lambda (pptx) pptx) (lambda (doc pptx) #t) (lambda (pptx) #t)
               always-open))

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
                "end tell"))
   (lambda () (process-running? "Microsoft PowerPoint"))))

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
     (define k (path->string (path-replace-extension (path->complete-path pptx) ".key")))
     ;; Opening a .pptx gives Keynote an unsaved import, so the first Cmd-S would
     ;; ask where to put it. Saving it here means Cmd-S just saves, and it saves
     ;; where the watcher is looking. Wrapped, because a Keynote that refuses is
     ;; not worth failing over -- the deck is still open and editable.
     (osascript "tell application \"Keynote\""
                "  repeat with d in documents"
                "    close d saving no"
                "  end repeat"
                (format "  set d to (open POSIX file \"~a\")" p)
                "  activate"
                "  try"
                (format "    save d in POSIX file \"~a\"" k)
                "  end try"
                "end tell"))
   (lambda () (process-running? "Keynote"))))

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
                     #t)))
   always-open))

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
;; Returns #t when the merge went through, which means *all* of it did. A save
;; is one thing: if any edit in it cannot be written, none of them is, and this
;; says so and fails. Writing some and reporting the rest would leave the
;; program and the deck each holding part of what was done.
;;
;; On a refusal the whole message is logged, not its first line: what to do
;; about it is usually on the lines after the first, and the editor is still
;; open on the slide that caused it.
(define (merge-back! program pptx document adapter workdir)
  (log! "deck changed -> merging into ~a\n" (file-name-from-path program))
  (with-handlers ([exn:fail? (lambda (e)
                               (log! "  ! merge refused\n")
                               (for ([l (in-list (string-split (exn-message e) "\n"))])
                                 (log! "    ~a\n" (string-trim l #:right? #f)))
                               #f)])
    ;; For an app that does not save .pptx, get one out of it first.
    (unless (equal? (path->string document) (path->string pptx))
      ((app-adapter-harvest! adapter) document pptx))
    (define r (sync-once program pptx #:workdir workdir #:atomic? #t))
    (define text (format-sync-report r))
    (for ([l (in-list (string-split text "\n"))]) (log! "~a\n" l))
    (define left (sync-report-skipped r))
    (cond
      [(null? left) #t]
      [else
       (log! "  ! nothing was merged: ~a of these could not be written\n" (length left))
       #f])))

;; Closing the editor ends the session, and so does Ctrl-C. Either way the last
;; edits are merged first and the scratch is cleared -- but only if that merge
;; went through. A refusal means the deck still holds something the program does
;; not, and deleting it would throw that away.
(define (finish! program pptx document adapter workdir why)
  (log! "~a\n" why)
  (define merged?
    (cond
      [(not (file-exists? document))
       (log! "  nothing left to merge\n")
       #t]
      [else (merge-back! program pptx document adapter workdir)]))
  (define scratch (scratch-dir-of program))
  (cond
    [(not merged?)
     (log! "  keeping ~a: the deck has edits this could not merge, and the
"
           (file-name-from-path scratch))
     (log! "  program was left as it was rather than take some of them\n")]
    [(not (directory-exists? scratch)) (void)]
    [else
     (delete-directory/files scratch #:must-exist? #f)
     (log! "  cleared ~a\n" (file-name-from-path scratch))])
  merged?)

;; The scratch beside a program, which is where the deck, the editor's document
;; and the agreed base live.
(define (scratch-dir-of program)
  (define full (path->complete-path program))
  (build-path (or (path-only full) (current-directory)) ".glide"))

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
                    #:open-check [open-check 10.0]
                    #:ticks [ticks #f])
  (define document ((app-adapter-document adapter) pptx))
  (log! "watching\n  program ~a\n  deck    ~a\n  app     ~a\n"
        program document (app-adapter-name adapter))
  ;; The deck can have moved while the watcher was not running -- stopped it,
  ;; edited a slide, started it again -- so those edits are merged before
  ;; anything is written. Regenerating first would have thrown them away.
  ;; Say when there is something to pick up, since the alternative reading of a
  ;; quiet start is that nothing was there.
  (define leftovers?
    (and (file-exists? (base-path-for program)) (file-exists? document)))
  (when leftovers?
    (log! "  ~a already holds a deck; merging what is in it before anything is written\n"
          (file-name-from-path (scratch-dir-of program))))
  (define start-stuck?
    (and leftovers? (not (merge-back! program pptx document adapter workdir))))
  ;; Then start from the program, so the deck exists and both sides agree.
  (unless start-stuck?
    (regenerate! program pptx adapter #:width w #:height h)
    (sync-once program pptx #:workdir workdir))
  ;; `stuck?` means the editor holds edits that were refused. Until they are
  ;; resolved the deck is not regenerated -- otherwise the obvious next move,
  ;; fixing the program as the message asks, would overwrite the very slides the
  ;; refusal was protecting. While stuck, a change on either side retries the
  ;; merge: the fix can be in the editor or in the program.
  ;; Asking the editor whether it is still open costs a subprocess, so it is
  ;; asked on a timer rather than every tick.
  (define open-every (max 1 (inexact->exact (round (/ open-check (max interval 0.01))))))
  (with-handlers ([exn:break? (lambda (_e)
                                (newline)
                                (finish! program pptx document adapter workdir
                                         "interrupted"))])
   (let loop ([prog-hash (content-hash program)]
             [doc-hash (content-hash document)]
             [stuck? start-stuck?]
             [n 0])
    (cond
      [(and ticks (>= n ticks)) (log! "done\n")]
      ;; The editor was closed, so the session is over.
      [(and (zero? (modulo n open-every)) (positive? n)
            (not ((app-adapter-open? adapter))))
       (finish! program pptx document adapter workdir
                (format "~a closed" (app-adapter-name adapter)))]
      [else
       (sleep interval)
       (define ph (content-hash program))
       (define dh (content-hash document))
       (define changed?
         (or (and ph (not (equal? ph prog-hash)))
             (and dh (not (equal? dh doc-hash)))))
       (cond
         [(and stuck? changed?)
          (settle document)
          (define ok? (merge-back! program pptx document adapter workdir))
          (unless ok?
            (log! "    the program is untouched and the deck is not being rewritten,
")
            (log! "    so nothing you have done in ~a is lost. To drop those edits
"
                  (app-adapter-name adapter))
            (log! "    and take the program as it is, delete ~a and save the program.
"
                  (file-name-from-path (base-path-for program))))
          (loop (content-hash program) (content-hash document) (not ok?) (add1 n))]
         [stuck? (loop prog-hash doc-hash #t (add1 n))]
         [(and ph (not (equal? ph prog-hash)))
          (settle program)
          (regenerate! program pptx adapter #:width w #:height h)
          ;; Re-read both, since we just wrote the deck ourselves.
          (loop (content-hash program) (content-hash document) #f (add1 n))]
         [(and dh (not (equal? dh doc-hash)))
          (settle document)
          (define ok? (merge-back! program pptx document adapter workdir))
          (unless ok?
            (log! "    the program is untouched and the deck will not be rewritten,
")
            (log! "    so nothing you have done in ~a is lost -- fix what it names,
"
                  (app-adapter-name adapter))
            (log! "    there or in the program, and save again.
"))
          (loop (content-hash program) (content-hash document) (not ok?) (add1 n))]
         [else (loop prog-hash doc-hash #f (add1 n))])]))))
