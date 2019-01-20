#lang racket/base

(provide surrogate%)
(require framework
         (prefix-in s: "surrogate.rkt"))

(define surrogate%
  (s:surrogate% racket:text-mode%))
