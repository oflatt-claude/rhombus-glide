#lang racket/base
;; The parent process, driven from both sides.
;;
;; Timing-sensitive by nature, so the assertions are about the state the loop
;; settles into rather than about how quickly it gets there.
(require rackunit racket/list racket/string racket/file racket/path racket/port
         racket/system racket/runtime-path
         glide-pptx/ir glide-pptx/parse glide-pptx/emit-racket glide-pptx/export
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
(define program (build-path dir "deck.rkt"))
(define pptx (build-path dir "deck.pptx"))

(define (write-program! color)
  (call-with-output-file program #:exists 'replace
    (lambda (o)
      (write-string
       (string-join
        (list "#lang racket/base"
              "(require pict glide-pptx/runtime)"
              "(provide all-slides slide-width slide-height)"
              "(define slide-width 480.0)"
              "(define slide-height 270.0)"
              "(define slide-1"
              "  (slide-canvas"
              "   #:width slide-width #:height slide-height #:background (hex \"FFFFFF\")"
              "   (at 40.0 60.0 #:tag \"Box\""
              "       (shape-pict #:width 100.0 #:height 40.0 #:shape \"roundRect\""
              (format "                   #:fill (hex ~s)))" color)
              "   (at 200.0 60.0 #:tag \"Label\""
              "       (textbox #:width 200.0 #:height 30.0"
              "                (para* (run* \"hello\" #:size 18.0))))))"
              "(define all-slides (list slide-1))"
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
                 (lambda () (regexp-match? #rx"at 150[.]0 90[.]0" (file->string program)))))
(define source-after (file->string program))
(check-true (regexp-match? #rx"at 150[.]0 90[.]0" source-after)
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
