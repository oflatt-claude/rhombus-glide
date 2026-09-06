#lang racket/base
;; The rest: everything that renders through LibreOffice to compare against, and
;; everything that sweeps the corpus. Four minutes or so, and it needs both of
;; those present.
;;
;; `tools/fetch-corpus.sh` downloads the decks; without them the corpus and
;; coverage modules say so and pass.
(require rackunit/log)
(require "export.rkt" "roundtrip.rkt" "fidelity.rkt" "elements.rkt" "render.rkt" "roundtrip-look.rkt"
         "coverage.rkt" "corpus.rkt"
         ;; Here rather than in the fast suite because it runs whole Rhombus
         ;; programs, and compiling one of those costs more than everything the
         ;; fast suite does.
         "staged.rkt")

;; A check that fails prints and carries on, which is what makes a whole run
;; readable -- and leaves the exit code saying nothing. Run on its own, this
;; says so; required by a suite, the suite says it once at the end.
(module+ main (void (test-log #:display? #t #:exit? #t)))
