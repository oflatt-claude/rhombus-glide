#lang racket/base
;; The parent process, driven from both sides.
;;
;; Timing-sensitive by nature, so the assertions are about the state the loop
;; settles into rather than about how quickly it gets there.
(require rackunit racket/list racket/string racket/file racket/path racket/port
         racket/system racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/emit-rhombus glide-pptx/export
         glide-pptx/sync glide-pptx/watch glide-pptx/runtime "deck-edit.rkt")

(define-runtime-path decks-dir "decks")

(define work (build-path (find-system-path 'temp-dir) "glide-pptx-watch"))
(delete-directory/files work #:must-exist? #f)
(make-directory* work)

;; --------------------------------------------------------- adapter surface

(check-true (hash-has-key? adapters 'keynote) "there is a Keynote adapter")
(check-true (hash-has-key? adapters 'powerpoint) "and a PowerPoint one")
(check-true (hash-has-key? adapters 'libreoffice) "and a LibreOffice one")
(check-equal? (app-adapter-name (adapter-named 'none)) 'none)
;; Keynote edits a .key beside the deck, and never the deck itself.
(check-equal? (path->string
               ((app-adapter-document (adapter-named 'keynote)) (string->path "/tmp/a.pptx")))
              "/tmp/a.key")
;; An app whose own format is .pptx edits the deck directly, with nothing to
;; harvest -- which is why it is the easier target.
(check-equal? ((app-adapter-document (adapter-named 'powerpoint)) (string->path "/tmp/a.pptx"))
              (string->path "/tmp/a.pptx"))
(check-exn exn:fail? (lambda () (adapter-named 'inkscape)) "an unknown app is refused")

;; ------------------------------------------------------------ a hand-written
;; A tiny program, so the test is about the loop rather than about a deck.

(define dir (build-path work "loop"))
(make-directory* dir)
(define program (build-path dir "deck.rhm"))
(define pptx (build-path dir "deck.pptx"))

(define (write-program! color)
  (call-with-output-file program #:exists 'replace
    (lambda (o)
      (write-string
       (string-join
        (list "#lang rhombus/and_meta"
              "import:"
              "  lib(\"glide-pptx/runtime.rhm\") open"
              "export:"
              "  all_slides"
              "  slide_width"
              "  slide_height"
              "def slide_width = 480.0"
              "def slide_height = 270.0"
              "def slide_1 = slide_canvas("
              "  ~width: slide_width, ~height: slide_height, ~background: hex(\"FFFFFF\"),"
              "  at(40.0, 60.0, ~tag: \"Box\","
              "     shape_pict(~width: 100.0, ~height: 40.0, ~shape: \"roundRect\","
              (format "                ~~fill: hex(~s)))," color)
              "  at(200.0, 60.0, ~tag: \"Label\","
              "     textbox(~width: 200.0, ~height: 30.0,"
              "             para(run(\"hello\", ~size: 18.0))))"
              ")"
              "def all_slides = [slide_1]"
              "")
        "\n") o))))

(void (write-program! "4472C4"))

;; --------------------------------------------------- one pass, deterministic

(check-true (and (parameterize ([current-watch-log void])
                   (watch-once program pptx #:adapter (adapter-named 'none)))
                 #t)
            "a single pass regenerates the deck")
(check-true (file-exists? pptx) "and the deck exists")

;; ------------------------------------------------------------ the whole loop

(define log-lines '())
(define (note fmt . args) (set! log-lines (cons (apply format fmt args) log-lines)))

(define watcher
  (thread (lambda ()
            (parameterize ([current-watch-log note])
              (watch-loop program pptx
                          #:adapter (adapter-named 'none)
                          #:workdir (build-path dir "w")
                          #:interval 0.15
                          #:ticks 120)))))

(define (wait-for! what pred [limit 15.0])
  (let loop ([waited 0.0])
    (cond
      [(pred) #t]
      [(>= waited limit) (fail (format "timed out waiting for ~a" what)) #f]
      [else (sleep 0.2) (loop (+ waited 0.2))])))

;; 1. The program is saved: the deck should be rewritten.
(sleep 1.0)
(define deck-before (file->bytes pptx))
(void (write-program! "ED7D31"))
(void (wait-for! "the deck to be regenerated"
                 (lambda () (not (equal? deck-before (file->bytes pptx))))))
(check-false (equal? deck-before (file->bytes pptx))
             "saving the program regenerated the deck")

;; 2. The deck is saved with a shape moved: the program should be patched.
(sleep 0.8)
(define source-before (file->string program))
(void (check-true (drag-in-deck! pptx 1 "Box" 150.0 90.0)
                  "the shape to drag was found in the generated deck"))

(void (wait-for! "the program to be patched"
                 (lambda () (regexp-match? #rx"at[(]150[.]0, 90[.]0" (file->string program)))))
(define source-after (file->string program))
(check-true (regexp-match? #rx"at[(]150[.]0, 90[.]0" source-after)
            "dragging in the deck moved the literal in the program")
(check-equal? (length (string-split source-before "\n"))
              (length (string-split source-after "\n"))
              "and changed no other line")
(check-true (regexp-match? #rx"ED7D31" source-after)
            "the program's own edit was left alone")

(void (kill-thread watcher))

(check-true (for/or ([l (in-list log-lines)]) (regexp-match? #rx"regenerating" l))
            "the loop reported regenerating the deck")
(check-true (for/or ([l (in-list log-lines)]) (regexp-match? #rx"merging" l))
            "and reported merging back")

(printf "watch tests done; artifacts under ~a\n" work)

;; The pieces the latch test needs, kept out of the test body.
(define (deck-slide-count pptx)
  (length (deck-slides (pptx->deck pptx #:workdir (make-temporary-file "wc~a" 'directory)))))

(define (merge-back-once program pptx workdir)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (sync-once program pptx #:workdir workdir)
    #t))

(define (merge-back-message program pptx workdir)
  (with-handlers ([exn:fail? exn-message])
    (sync-once program pptx #:workdir workdir)
    ""))

;; ------------------------------------- a refused merge protects the deck

;; The trap this closes: a refused merge left the editor's edits sitting there,
;; and then the obvious next move -- fixing the program as the message asks --
;; regenerated the deck and threw those edits away. So a refusal latches: while
;; it stands the deck is not rewritten, and a change on either side retries.
;;
;; A save is one thing: if any edit in it cannot be written, none of them is.
;; So the refusals used here are ones that stand -- a tag on two `at` forms,
;; which an edit could land on either of, and a colour two shapes share
;; recoloured on one of them.
(let ()
  (define dir (build-path work "stuck"))
  (make-directory* dir)
  (define program (build-path dir "deck.rhm"))
  (define pptx (build-path dir "deck.pptx"))
  (define (write-program! color #:duplicate-tag? [duplicate-tag? #f])
    (call-with-output-file program #:exists 'replace
      (lambda (o)
        (write-string
         (string-join
          (append
           (list "#lang rhombus/and_meta"
                 "import:"
                 "  lib(\"glide-pptx/runtime.rhm\") open"
                 "export:"
                 "  all_slides"
                 (format "def brand = hex(~s)" color)
                 "def slide_1 = slide_canvas("
                 "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
                 "  at(40.0, 60.0, ~tag: \"Box\","
                 "     shape_pict(~width: 100.0, ~height: 40.0, ~fill: brand)),"
                 "  at(200.0, 60.0, ~tag: \"Twin\","
                 (format "     shape_pict(~~width: 100.0, ~~height: 40.0, ~~fill: brand))~a"
                         (if duplicate-tag? "," "")))
           ;; A second `at` under the same tag: an edit under it could land on
           ;; either, so the merge refuses the program rather than guessing.
           (if duplicate-tag?
               (list "  at(200.0, 60.0, ~tag: \"Box\","
                     "     shape_pict(~width: 60.0, ~height: 40.0, ~fill: hex(\"ED7D31\")))")
               '())
           (list ")"
                 "def slide_2 = slide_canvas("
                 "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
                 "  at(60.0, 90.0, ~tag: \"Other\","
                 "     shape_pict(~width: 80.0, ~height: 30.0, ~fill: hex(\"70AD47\")))"
                 ")"
                 "def all_slides = [slide_1, slide_2]"
                 ""))
          "\n") o))))
  (write-program! "4472C4")

  ;; One pass to lay the deck down and record a base.
  (watch-once program pptx #:width 480.0 #:height 270.0)
  (sync-once program pptx #:workdir (build-path dir "w"))
  (check-equal? (deck-slide-count pptx) 2 "the deck starts with the program's two slides")

  ;; A program the merge cannot read at all: two `at` forms answering to one
  ;; tag, so an edit could land on either. The whole message comes through,
  ;; because what to do about it is not on the first line.
  (write-program! "4472C4" #:duplicate-tag? #t)
  (check-false (merge-back-once program pptx (build-path dir "w"))
               "and merging into it is refused")
  (define msg (merge-back-message program pptx (build-path dir "w")))
  (check-regexp-match #rx"Box" msg)
  (check-regexp-match #rx"different tags" msg "the message keeps its later lines")
  (write-program! "4472C4")

  ;; Now the trap, with the loop actually running: the refusal has to happen
  ;; inside a run and a program save has to follow it in the same run.
  (define lines '())
  (define (note fmt . args) (set! lines (cons (apply format fmt args) lines)))
  (define (said? rx) (ormap (lambda (l) (regexp-match? rx l)) lines))
  (define (wait! what pred [limit 20.0])
    (let loop ([waited 0.0])
      (cond [(pred) #t]
            [(>= waited limit) (fail (format "timed out waiting for ~a" what)) #f]
            [else (sleep 0.1) (loop (+ waited 0.1))])))

  ;; Back to agreement first: the loop refuses to start on a deck it cannot
  ;; merge, which is right but is not what this is testing.
  (watch-once program pptx #:width 480.0 #:height 270.0)
  (void (sync-once program pptx #:workdir (build-path dir "w")))
  (check-equal? (deck-slide-count pptx) 2 "the deck is whole again")

  (define runner
    (thread (lambda ()
              (parameterize ([current-watch-log note])
                (watch-loop program pptx #:width 480.0 #:height 270.0
                            #:adapter (adapter-named 'none)
                            #:workdir (build-path dir "w")
                            #:interval 0.05 #:ticks 400)))))
  ;; Startup unzips, merges and regenerates, which is slower than any sleep
  ;; worth writing -- so wait for it to say it wrote the deck.
  (wait! "the loop to finish starting" (lambda () (said? #rx"slides written")))
  (sleep 0.3)
  ;; Two edits in one save, one of which cannot be written: the resize can be,
  ;; and recolouring one of the two shapes that share `brand` cannot. So the
  ;; save fails whole -- and the resize is still in the deck, because the deck
  ;; was never rewritten from a program that never took it.
  (check-true (resize-in-deck! pptx 1 "Box" 200.0 80.0) "a shape was resized")
  (check-true (edit-after-tag! pptx 1 "Twin" #px"<a:srgbClr val=\"[0-9A-Fa-f]+\"/>"
                               "<a:srgbClr val=\"70AD47\"/>")
              "and one of the two shapes sharing a colour was recoloured")
  (wait! "the save to fail whole" (lambda () (said? #rx"nothing was merged")))
  ;; Then save the program, which is what the message asks for.
  (write-program! "70AD47")
  (wait! "the loop to say the program is untouched" (lambda () (said? #rx"untouched")))
  (sleep 0.4)
  (check-regexp-match #rx"cx=\"2540000\""
                      (deck-part pptx "ppt/slides/slide1.xml")
                      "the resize survived a program save -- the deck was not regenerated")
  (check-false (regexp-match? #rx"~width: 200[.]0" (file->string program))
               "and the program never took it, since the save failed whole")
  (kill-thread runner))

;; ------------------------------------------- closing the editor ends the session

;; Quitting the editor should end the session and take the scratch with it --
;; the deck, the editor's own document and the agreed base are all derived, and
;; leaving them behind is what the `.glide` folder was complained about for. The
;; last edits are merged first, though, and a merge that is refused keeps the
;; scratch: the deck then holds something the program does not.
(define (closing-adapter open-box)
  (app-adapter 'test (lambda (pptx) pptx) (lambda (doc pptx) #t) (lambda (pptx) #t)
               (lambda () (unbox open-box))))

(let ()
  (define dir (build-path work "closing"))
  (make-directory* dir)
  (define program (build-path dir "deck.rhm"))
  (display-to-file
   (string-join
    (list "#lang rhombus/and_meta"
          "import:"
          "  lib(\"glide-pptx/runtime.rhm\") open"
          "export:"
          "  all_slides"
          "def slide_1 = slide_canvas("
          "  ~width: 480.0, ~height: 270.0, ~background: hex(\"FFFFFF\"),"
          "  at(40.0, 60.0, ~tag: \"Box\","
          "     shape_pict(~width: 100.0, ~height: 40.0, ~fill: hex(\"4472C4\")))"
          ")"
          "def all_slides = [slide_1]"
          "")
    "\n")
   program #:exists 'replace)
  (define scratch (scratch-dir-of program))
  (define pptx (build-path scratch "deck.pptx"))
  (make-directory* scratch)

  (define open? (box #t))
  (define lines '())
  (define (note fmt . args) (set! lines (cons (apply format fmt args) lines)))
  (define (said? rx) (ormap (lambda (l) (regexp-match? rx l)) lines))

  ;; A session that is running, then closed.
  (define runner
    (thread (lambda ()
              (parameterize ([current-watch-log note])
                (watch-loop program pptx #:width 480.0 #:height 270.0
                            #:adapter (closing-adapter open?)
                            #:workdir (build-path dir "w")
                            #:interval 0.05 #:open-check 0.1 #:ticks 400)))))
  (let wait ([n 0])
    (cond [(said? #rx"slides written") (void)]
          [(> n 200) (fail "the session never started")]
          [else (sleep 0.1) (wait (add1 n))]))
  (check-true (file-exists? pptx) "the deck is in scratch while the session runs")

  (set-box! open? #f)
  (let wait ([n 0])
    (cond [(said? #rx"cleared") (void)]
          [(> n 200) (fail "the session never finished")]
          [else (sleep 0.1) (wait (add1 n))]))
  (check-true (said? #rx"test closed") "closing the editor is what ended it")
  (check-false (directory-exists? scratch) "and the scratch went with it")
  (check-true (file-exists? program) "the program is what is left")
  (kill-thread runner))
