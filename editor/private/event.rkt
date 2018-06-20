#lang racket/base

(provide (all-defined-out))
(require racket/gui/base
         racket/class)

(define text-change-event%
  (class event%
    (super-new)
    (init-field [text ""])
    (define/public (get-text)
      text)
    (define/public (set-text! t)
      (set! text t))))
