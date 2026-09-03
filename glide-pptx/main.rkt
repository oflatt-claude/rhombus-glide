#lang racket/base
;; Command line entry point: `raco glide-pptx <subcommand> ...`.
(require racket/cmdline racket/list racket/string racket/file racket/path racket/treelist pict
         racket/pretty racket/format
         "ir.rkt" "parse.rkt" "render.rkt" "runtime.rkt"
         "emit-rhombus.rkt" "verify.rkt" "geometry.rkt"
         "export.rkt" "sync.rkt" "watch.rkt"
         (only-in "semantic.rkt" current-flatten-opaque?)
         (only-in "parse.rkt" current-allow-unsupported?))
(provide main)

;; A deck has to be unzipped to be read, and the images are read from there
;; while the slides draw -- so there is a scratch directory for the length of a
;; command. It goes next to the output rather than in the system temp dir so that
;; `--keep-work` can leave it somewhere findable.
(define (default-workdir out) (build-path out ".glide-pptx"))

(define keep-work? (make-parameter #f))

(define (clean-work! dir)
  (if (keep-work?)
      (printf "scratch kept in ~a\n" dir)
      (delete-directory/files dir #:must-exist? #f)))

;; A Keynote document is not a .pptx, but Keynote will write one -- and the
;; watcher already knows how to ask it to. So a `.key` is accepted anywhere a
;; deck is, by exporting it first.
(define (as-pptx path out)
  (cond
    [(not (regexp-match? #rx"[.](key|keynote)$"
                         (string-downcase (if (path? path) (path->string path) path))))
     path]
    [else
     (define full (path->complete-path path))
     (define exported (build-path (default-workdir out)
                                  (string-append (deck-stem full) ".pptx")))
     (make-directory* (default-workdir out))
     (printf "exporting ~a from Keynote\n" (file-name-from-path full))
     ((app-adapter-harvest! (adapter-named 'keynote)) full exported)
     (unless (file-exists? exported)
       (error 'glide-pptx
              (string-append "Keynote did not write ~a.\n"
                             "  Export it by hand -- File > Export To > PowerPoint -- and"
                             " pass the .pptx.")
              exported))
     exported]))

(define (with-deck given out proc)
  (define pptx (as-pptx given out))
  (define warnings (box '()))
  (define d (parameterize ([current-warnings warnings])
              (pptx->deck pptx #:workdir (build-path (default-workdir out)
                                                     (deck-stem pptx)))))
  (begin0 (parameterize ([runtime-warnings warnings]) (proc d warnings))
          (report-warnings warnings)
          ;; Everything the output needs has been copied out by now.
          (clean-work! (default-workdir out))))

(define (deck-stem pptx)
  (path->string (path-replace-extension (file-name-from-path pptx) "")))

(define (report-warnings warnings)
  (define ws (remove-duplicates (reverse (unbox warnings))))
  (unless (null? ws)
    (eprintf "~a note~a about approximate rendering:\n" (length ws)
             (if (= 1 (length ws)) "" "s"))
    (for ([w (in-list ws)]) (eprintf "  - ~a\n" w))))

;; ------------------------------------------------------------- subcommands

(define (cmd-translate args)
  (define out-dir (box "out"))
  (define files
    (parse-command-line
     "glide-pptx translate" args
     `((once-each
        [("-o" "--out") ,(lambda (_ v) (set-box! out-dir v)) ("Output directory" "dir")]))
     (lambda (_ . fs) fs)
     '("deck.pptx")))
  ;; `-o` names a directory, but `-o talk.rhm` clearly means the file, so both
  ;; work: the images go beside whichever it is.
  (define named-file?
    (regexp-match? #rx"[.]rhm$" (unbox out-dir)))
  (define dir (if named-file?
                  (let ([p (path-only (path->complete-path (unbox out-dir)))])
                    (if p (path->string p) "."))
                  (unbox out-dir)))
  (for ([f (in-list files)])
    (with-deck f dir
      (lambda (d _w)
        (define path
          (if named-file?
              (path->complete-path (unbox out-dir))
              (build-path dir (string-append (deck-stem f) ".rhm"))))
        (write-rhombus-deck d path #:source-name f)
        (printf "~a  (~a slides, ~a elements)\n"
                path (length (deck-slides d)) (deck-count-elements d))))))

(define (cmd-render args)
  (define out-dir (box "out"))
  (define files
    (parse-command-line
     "glide-pptx render" args
     `((once-each
        [("-o" "--out") ,(lambda (_ v) (set-box! out-dir v)) ("Output directory" "dir")]))
     (lambda (_ . fs) fs)
     '("deck.pptx")))
  (for ([f (in-list files)])
    (with-deck f (unbox out-dir)
      (lambda (d _w)
        (make-directory* (unbox out-dir))
        (define path (build-path (unbox out-dir) (string-append (deck-stem f) ".pdf")))
        (picts->pdf (deck->picts d) path #:width (deck-width d) #:height (deck-height d))
        (printf "~a  (~a pages)\n" path (length (deck-slides d)))))))

(define (cmd-ir args)
  (define files
    (parse-command-line "glide-pptx ir" args '() (lambda (_ . fs) fs) '("deck.pptx")))
  (for ([f (in-list files)])
    (with-deck f "out"
      (lambda (d _w)
        (pretty-write d)))))

(define (cmd-verify args)
  (define out-dir (box "out/verify"))
  (define dpi (box 96))
  (define mae (box 0.02))
  (define bad (box 0.06))
  (define files
    (parse-command-line
     "glide-pptx verify" args
     `((once-each
        [("-o" "--out") ,(lambda (_ v) (set-box! out-dir v)) ("Where to put images" "dir")]
        [("--dpi") ,(lambda (_ v) (set-box! dpi (string->number v))) ("Raster resolution" "n")]
        [("--mae") ,(lambda (_ v) (set-box! mae (string->number v)))
                   ("Max mean error, 0..1" "x")]
        [("--bad") ,(lambda (_ v) (set-box! bad (string->number v)))
                   ("Max fraction of differing pixels, 0..1" "x")]))
     (lambda (_ . fs) fs)
     '("deck.pptx")))
  (define results
    (for/list ([f (in-list files)])
      (define warnings (box '()))
      (define dd (verify-deck f (unbox out-dir) #:dpi (unbox dpi)
                              #:mae-threshold (unbox mae)
                              #:bad-threshold (unbox bad)
                              #:warnings warnings))
      (display (format-report dd #:mae-threshold (unbox mae) #:bad-threshold (unbox bad)))
      (report-warnings warnings)
      dd))
  (define failed (filter (lambda (dd) (not (deck-diff-ok? dd))) results))
  (printf "\n~a of ~a deck~a within tolerance (mean err <= ~a%, pixels off <= ~a%)\n"
          (- (length results) (length failed)) (length results)
          (if (= 1 (length results)) "" "s")
          (* 100 (unbox mae)) (* 100 (unbox bad)))
  (when (pair? failed) (exit 1)))

;; Any module that provides slide picts can be exported, whether or not it was
;; generated by this tool: `all-slides` is the convention the emitters follow,
;; and `all_slides` is its Rhombus spelling.
(define (cmd-export args)
  (define out (box #f))
  (define width (box #f))
  (define height (box #f))
  (define binding (box #f))
  (define slideshow? (box #f))
  (define flatten? (box #t))
  (define files
    (parse-command-line
     "glide-pptx export" args
     `((once-each
        [("-o" "--out") ,(lambda (_ v) (set-box! out v)) ("Output .pptx" "file")]
        [("--width") ,(lambda (_ v) (set-box! width (string->number v)))
                     ("Slide width in points" "pt")]
        [("--height") ,(lambda (_ v) (set-box! height (string->number v)))
                      ("Slide height in points" "pt")]
        [("--slides") ,(lambda (_ v) (set-box! binding v))
                      ("Name of the provided list of picts" "id")]
        [("--slideshow") ,(lambda (_) (set-box! slideshow? #t))
                         ("Run as a slideshow program: one slide per advance")]
        [("--no-flatten") ,(lambda (_) (set-box! flatten? #f))
                          ("Keep unsyncable elements as separate shapes")]))
     (lambda (_ . fs) fs)
     '("program.rkt")))
  (for ([f (in-list files)])
    (define full (path->complete-path f))
    (define picts
      (if (unbox slideshow?)
          (slideshow-slides full (unbox width) (unbox height))
          (load-program-picts full #:named (unbox binding))))
    (define target
      (or (unbox out)
          (path->string (path-replace-extension (file-name-from-path f) ".pptx"))))
    (define warnings (box '()))
    (parameterize ([current-export-warnings warnings]
                   [current-flatten-opaque? (unbox flatten?)])
      (picts->pptx picts target #:width (unbox width) #:height (unbox height)))
    (printf "~a  (~a slide~a)\n" target (length picts) (if (= 1 (length picts)) "" "s"))
    (report-warnings warnings)))

;; A `slideshow` program does not provide a list of slides; it *calls* `slide`,
;; and each animation step is a separate frame. `get-slides-as-picts` is the
;; same entry point `raco slideshow --pdf` uses, so one pptx slide comes out per
;; step -- which for an animated Rhombus talk is one per epoch.
;;
;; It is required lazily because it pulls in the GUI toolkit, and nothing else
;; here needs that.
(define SLIDESHOW-W 1360.0)
(define SLIDESHOW-H 766.0)

(define (slideshow-slides path width height)
  (define get
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (error 'export
                              (string-append
                               "--slideshow needs slideshow and its GUI support:\n"
                               "  ~a")
                              (first (string-split (exn-message e) "\n"))))])
      (dynamic-require 'slideshow/slides-to-picts 'get-slides-as-picts)))
  (define w (or width 960.0))
  (define h (or height 540.0))
  ;; Always condensed. An animated pict is many frames between advances, so
  ;; without this a 291-slide talk comes out as 4869 slides and 135 MB.
  (define frames
    (parameterize ([current-directory (path-only path)])
      (get (path->string path) SLIDESHOW-W SLIDESHOW-H #t)))
  ;; Slideshow renders at its own size, so the frames are scaled onto the page.
  (for/list ([p (in-list frames)])
    (scale p (/ w SLIDESHOW-W) (/ h SLIDESHOW-H))))



;; One merge pass between a program and a deck. With no base recorded yet this
;; only writes the base, since there is nothing to merge against.
(define (cmd-sync args)
  (define dry (box #f))
  (define files
    (parse-command-line
     "glide-pptx sync" args
     `((once-each
        [("-n" "--dry-run") ,(lambda (_) (set-box! dry #t))
                            ("Report what would change without touching anything")]))
     (lambda (_ program deck) (list program deck))
     '("program.rkt" "deck.pptx")))
  (define program (first files))
  ;; A `.key` is accepted here as well: Keynote writes the .pptx to merge from.
  (define deck (as-pptx (second files)
                        (let ([p (path-only (path->complete-path program))])
                          (if p (path->string p) "."))))
  (define r
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "~a <-> ~a\n" program deck)
                                 (eprintf "~a\n" (exn-message e))
                                 (exit 1))])
      (sync-once program deck #:dry-run? (unbox dry))))
  (printf "~a <-> ~a\n" program deck)
  (display (format-sync-report r))
  (when (sync-report-base-written? r)
    (printf "  base: ~a\n" (base-path-for program))))

;; The parent process: regenerate the deck when the program is saved, and merge
;; the deck's geometry back when it is saved.
(define (cmd-watch args)
  (define out (box #f))
  (define app (box 'none))
  (define width (box #f))
  (define height (box #f))
  (define once (box #f))
  (define files
    (parse-command-line
     "glide-pptx watch" args
     `((once-each
        [("-o" "--out") ,(lambda (_ v) (set-box! out v)) ("Deck to generate" "file")]
        [("--app") ,(lambda (_ v) (set-box! app (string->symbol v)))
                   ("keynote, powerpoint, libreoffice or none" "name")]
        [("--width") ,(lambda (_ v) (set-box! width (string->number v)))
                     ("Slide width in points" "pt")]
        [("--height") ,(lambda (_ v) (set-box! height (string->number v)))
                      ("Slide height in points" "pt")]
        [("-1" "--once") ,(lambda (_) (set-box! once #t))
                         ("Regenerate once and exit, without watching")]))
     (lambda (_ program) (list program))
     '("program.rkt")))
  (define program (first files))
  (define pptx (or (unbox out)
                   (path->string (path-replace-extension
                                  (file-name-from-path program) ".pptx"))))
  (define adapter (adapter-named (unbox app)))
  (if (unbox once)
      (watch-once program pptx #:adapter adapter
                  #:width (unbox width) #:height (unbox height))
      (watch-loop program pptx #:adapter adapter
                  #:width (unbox width) #:height (unbox height))))

(define (cmd-presets args)
  (printf "~a preset geometries drawn exactly:\n" (length (preset-names)))
  (for ([g (in-list (preset-names))]) (printf "  ~a\n" g))
  (printf "Anything else is drawn as a rectangle and reported as a note.\n"))

(define subcommands
  (list (list "translate" "emit a Pict program from a deck" cmd-translate)
        (list "export" "write a .pptx from a program's slide picts" cmd-export)
        (list "render" "render a deck straight to PDF" cmd-render)
        (list "sync" "merge a deck's edits back into a program" cmd-sync)
        (list "watch" "keep a program and a deck in step, both ways" cmd-watch)
        (list "verify" "compare our PDF against LibreOffice's" cmd-verify)
        (list "ir" "print the intermediate representation" cmd-ir)
        (list "presets" "list the shape geometries we draw exactly" cmd-presets)))

(define (usage)
  (printf "usage: raco glide-pptx <command> [options] deck.pptx ...\n\n")
  (for ([s (in-list subcommands)])
    (printf "  ~a~a\n" (~a (first s) #:min-width 12) (second s)))
  (printf "\nRun a command with --help for its options.\n")
  (printf "\n  --allow-unsupported  draw charts and diagrams as empty boxes\n")
  (printf "                       instead of refusing the deck\n")
  (printf "  --keep-work          leave the unzipped deck behind for inspection\n"))

(define (main . argv)
  (define args (if (and (= 1 (length argv)) (vector? (car argv)))
                   (vector->list (car argv))
                   argv))
  ;; This one is not per-command: every command that reads a deck honours it,
  ;; and it says "I am only looking", not "this deck is safe to round-trip".
  (define allow? (and (member "--allow-unsupported" args) #t))
  (set! args (remove "--allow-unsupported" args))
  (define keep? (and (member "--keep-work" args) #t))
  (set! args (remove "--keep-work" args))
  (cond
    [(or (null? args) (member (car args) '("-h" "--help" "help"))) (usage)]
    [(assoc (car args) subcommands)
     => (lambda (s)
          (parameterize ([current-allow-unsupported? allow?]
                         [keep-work? keep?]
                         [current-keep-work? keep?])
            ((third s) (list->vector (cdr args)))))]
    [else (eprintf "unknown command: ~a\n\n" (car args)) (usage) (exit 2)]))

;; `raco` runs a registered command by requiring its module, so the dispatch
;; happens at module level rather than in a `main` submodule.
(apply main (vector->list (current-command-line-arguments)))
