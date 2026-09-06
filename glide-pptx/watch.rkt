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
         racket/system racket/port file/sha1 racket/runtime-path
         "export.rkt" "sync.rkt")
(provide (struct-out app-adapter) adapters adapter-named scratch-dir-of
         watch-loop watch-once program-picts
         soffice-exe powerpoint-installed?
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
   ;; Reloading means closing and reopening, which puts the editor back on the
   ;; first slide -- and a regeneration happens every time the program is
   ;; saved, so that is once a keystroke. The slide being looked at is read
   ;; first and set again after, so the view stays where it was. Every step of
   ;; that is wrapped: an editor that will not answer should cost the reload,
   ;; not the session.
   (lambda (pptx)
     (define p (path->string (path->complete-path pptx)))
     (osascript "tell application \"Microsoft PowerPoint\""
                (format "  set target to POSIX file \"~a\"" p)
                "  set showing to 1"
                "  try"
                "    repeat with w in document windows"
                (format "      if (full name of (presentation of w)) is \"~a\" then" p)
                "        set showing to slide index of view of w"
                "      end if"
                "    end repeat"
                "  end try"
                "  repeat with d in presentations"
                (format "    if (full name of d) is \"~a\" then close d saving no" p)
                "  end repeat"
                "  open target"
                "  activate"
                "  try"
                "    set slide index of view of document window 1 to showing"
                "  end try"
                "end tell"))
   (lambda () (process-running? "Microsoft PowerPoint"))))

;; Keynote's own format is a .key bundle and it never saves .pptx, so the deck
;; has to be exported out of it before a merge can read it, and a regenerated
;; deck has to be re-imported. Keynote has no reload, so the document is closed
;; and reopened: the slide being looked at is read first and set again after,
;; and the selection is lost either way. Untested here.
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
                ;; Which slide is being looked at, so reopening puts it back
                ;; there rather than at the beginning.
                "  set showing to 1"
                "  try"
                "    if (count of documents) > 0 then"
                "      set showing to slide number of current slide of document 1"
                "    end if"
                "  end try"
                "  repeat with d in documents"
                "    close d saving no"
                "  end repeat"
                (format "  set d to (open POSIX file \"~a\")" p)
                "  activate"
                "  try"
                (format "    save d in POSIX file \"~a\"" k)
                "  end try"
                "  try"
                "    if showing > 0 and showing <= (count of slides of d) then"
                "      set current slide of d to slide showing of d"
                "    end if"
                "  end try"
                "end tell"))
   (lambda () (process-running? "Keynote"))))

;; LibreOffice edits .pptx natively, so the document is the deck, as with
;; PowerPoint. What it does not have is a reload: opening a file it already has
;; open raises the window it is already showing, whatever is on disk now. A
;; deck is regenerated every time the program is saved, so left at that the
;; editor shows a deck from several edits ago -- and a save from there writes
;; that back over the program's work, which the merge then reads as a pile of
;; edits undoing everything. So it is asked over UNO instead, which is the
;; interface it does have, and the slide in view is kept across the reload.
;;
;; It runs on a profile of its own, under the scratch beside the program. That
;; is what makes the socket certain -- LibreOffice is one process per profile,
;; and a copy the user already had running would otherwise swallow the launch
;; and have no socket -- and it is somewhere to say, once, that saving a .pptx
;; as a .pptx needs no warning.
(define-runtime-path libreoffice-driver "libreoffice.py")

(define LIBREOFFICE-PORT 2143)

;; On a Mac it is inside the application bundle rather than on the PATH, which
;; is where a `raco glide` run would otherwise conclude there is no LibreOffice
;; and reach for PowerPoint instead.
(define MAC-SOFFICE "/Applications/LibreOffice.app/Contents/MacOS/soffice")

;; Whether there is a PowerPoint to drive. Without one, `tell application
;; "Microsoft PowerPoint"` cannot even be compiled -- AppleScript has no
;; dictionary to read `full name` out of -- and every reload fails with a
;; syntax error about a property, which says nothing about what is wrong.
(define (powerpoint-installed?)
  (and (eq? 'macosx (system-type 'os))
       (directory-exists? "/Applications/Microsoft PowerPoint.app")))

(define (soffice-exe)
  (or (find-executable-path "soffice")
      (find-executable-path "libreoffice")
      (and (file-exists? MAC-SOFFICE) (string->path MAC-SOFFICE))))

;; UNO comes from LibreOffice's own Python. A Mac's `python3` is the system's
;; and knows nothing about it, and even on Linux the packaged one is surer than
;; whatever `python3` happens to mean today.
(define (libreoffice-python)
  (define bundled
    (list "/Applications/LibreOffice.app/Contents/MacOS/python"
          "/Applications/LibreOffice.app/Contents/Resources/python"
          "/usr/lib/libreoffice/program/python"))
  (or (for/or ([p (in-list bundled)]) (and (file-exists? p) (string->path p)))
      (find-executable-path "python3")))

;; The helper says what it managed: 0 did it, 3 could not connect, 4 the
;; document is not open. Anything else, including no python-uno to run it
;; with, is "cannot tell".
(define (libreoffice-driver! what pptx)
  (define py (libreoffice-python))
  (and py
       (let ([out (open-output-string)])
         (parameterize ([current-output-port out] [current-error-port out])
           (system*/exit-code py (path->string libreoffice-driver) what
                              (number->string LIBREOFFICE-PORT)
                              (path->string (path->complete-path pptx)))))))

(define (libreoffice-launch! pptx)
  (define exe (soffice-exe))
  ;; Started on the person's own LibreOffice, with their settings and none of
  ;; the dialogs a fresh profile puts up -- and a fresh profile puts up a
  ;; welcome wizard, which is modal, which means nothing can be asked of the
  ;; document underneath it and a reload goes nowhere.
  ;;
  ;; The socket is what the reload is asked over. LibreOffice is one process
  ;; per profile, so a copy already running without one swallows this launch
  ;; and there is nothing to ask: then the deck is still written, still opens,
  ;; and only the reload is lost -- which the loop says out loud rather than
  ;; leaving a stale deck on screen looking current.
  ;; Argument by argument rather than as a command line: a talk kept in a
  ;; folder with a space in its name is not an unusual thing to have.
  (and exe
       (begin
         (process*/ports
          (open-output-nowhere) #f (open-output-nowhere)
          exe "--norestore"
          (format "--accept=socket,host=localhost,port=~a;urp;" LIBREOFFICE-PORT)
          (path->string (path->complete-path pptx)))
         #t)))

(define libreoffice-adapter
  (app-adapter
   'libreoffice
   (lambda (pptx) pptx)
   (lambda (doc pptx) #t)
   (lambda (pptx)
     ;; Reload what is open; open it if it is not. A driver that cannot say
     ;; either way falls back to the launch, which at least shows something.
     (case (libreoffice-driver! "reload" pptx)
       [(0) #t]
       [else (libreoffice-launch! pptx)]))
   ;; Only the driver can say; without it the session runs until it is
   ;; interrupted, which is what every other adapter that cannot tell does.
   (lambda ()
     (define deck (current-libreoffice-deck))
     (case (and deck (libreoffice-driver! "open" deck))
       [(4) #f]
       [else #t]))))

;; Which deck `open?` should ask about. The adapter's own `open?` takes no
;; argument -- it is asked about the session, and there is one deck in it.
(define current-libreoffice-deck (make-parameter #f))

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
    ;; Until it settles. One pass can leave work for the next -- an edit
    ;; inside a form another edit rewrote wholesale is left for a second look,
    ;; and grouping changes the drawing order -- and the deck is written again
    ;; from the program as soon as this returns, so anything still outstanding
    ;; would be written over rather than kept.
    (let loop ([pass 1])
      (define r (sync-once program pptx #:workdir workdir #:atomic? #t))
      ;; A later pass that found nothing is the usual case and says nothing.
      (when (or (= pass 1) (pair? (sync-report-actions r)))
        (for ([l (in-list (string-split (format-sync-report r) "\n"))]) (log! "~a\n" l)))
      (define left (sync-report-skipped r))
      (cond
        [(pair? left)
         (log! "  ! nothing was merged: ~a of these could not be written\n" (length left))
         #f]
        [(or (null? (sync-report-applied r)) (>= pass 3)) #t]
        [else (loop (add1 pass))]))))

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
  ;; Which deck this session is about, for an adapter that has to ask the
  ;; editor whether it is still open.
  (current-libreoffice-deck pptx)
  (log! "watching\n  program ~a\n  deck    ~a\n  app     ~a\n"
        program document (app-adapter-name adapter))
  ;; The program is the truth, so a session starts from it. Anything left in
  ;; the scratch is from a session that ended without merging -- a crash, or a
  ;; deck opened behind glide's back -- and merging it would be merging edits
  ;; against a program that has moved on since. It goes, and the deck is
  ;; written again from the program.
  (define scratch (scratch-dir-of program))
  (when (and (directory-exists? scratch) (file-exists? (base-path-for program)))
    (log! "  clearing ~a, left from a session that did not finish\n"
          (file-name-from-path scratch))
    (delete-directory/files scratch #:must-exist? #f))
  (regenerate! program pptx adapter #:width w #:height h)
  (sync-once program pptx #:workdir workdir)
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
             [stuck? #f]
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
