#lang racket/base

(provide surrogate%)
(require racket/class
         racket/list
         framework)

(define surrogate%
  (class racket:text-mode%
    (super-new)
    (define to-convert '())
    (define/override (on-change-style orig call-inner start end)
      (define text (send orig get-text start (+ start end)))
      (when (equal? text "#editor")
        (set! to-convert (cons (list start end text) to-convert)))
      (call-inner start end))
    (define/override (after-edit-sequence orig call-inner)
      (define conv-list (sort to-convert > #:key first))
      (set! to-convert '())
      (for/list ([i (in-list conv-list)])
        (displayln i))
      (call-inner))))
