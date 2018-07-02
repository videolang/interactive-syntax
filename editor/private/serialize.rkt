#lang racket/base

(provide deserialize-editor/vector)
(require racket/serialize)

(define deserialize-editor/vector
  (make-deserialize-info
   (λ (sup table)
     (vector sup table))
   (λ ()
     (define vec (vector #f #f))
     (values
      vec
      (λ (other)
        (vector-copy! vec 0 other))))))
