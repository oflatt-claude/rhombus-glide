#lang racket/base
;; Everything. `raco test tests/fast.rkt` is the one to run while working -- it
;; needs nothing but Racket and takes about a minute and a half; this adds the
;; comparisons against LibreOffice and the sweep over the corpus.
(require "fast.rkt" "slow.rkt")
