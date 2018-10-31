#lang racket/base

(provide (all-defined-out))
(require racket/match
         racket/fasl)

(define (expansion-monitor callback directory source custodian)
  (define receiver (make-log-receiver (current-logger)
                                      'info
                                      'editor-lex-for-editors))
  (thread
   (λ ()
     (let loop ()
       (define val (sync receiver))
       (match val
         [(vector level message
                  (vector elaborator editor src
                          line col pos new-line new-col new-pos)
                  name)
          (callback (vector (s-exp->fasl (syntax->datum elaborator))
                            (s-exp->fasl (syntax->datum editor))
                            pos new-pos))]
         [_ (void)])
       (loop)))))
