#lang racket/base
;; The rest: everything that renders through LibreOffice to compare against, and
;; everything that sweeps the corpus. Four minutes or so, and it needs both of
;; those present.
;;
;; `tools/fetch-corpus.sh` downloads the decks; without them the corpus and
;; coverage modules say so and pass.
(require "export.rkt" "roundtrip.rkt" "fidelity.rkt" "elements.rkt"
         "coverage.rkt" "corpus.rkt")
