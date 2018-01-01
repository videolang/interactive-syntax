#lang racket/base

(provide (all-defined-out))

(require racket/port
         racket/list
         syntax/readerr)

(define (make-editor-readtable #:readtable [base-readtable (current-readtable)])
  (define read-editor
    (case-lambda
      [(ch port)
       (define next (peek-bytes 5 0 port))
       (cond [(equal? next #"ditor")
              (read-bytes 5 port)
              (read/recursive port)]
             [else
              (define-values (in out) (make-pipe))
              (write-bytes #"#e" out)
              (close-output-port out)
              (read/recursive (input-port-append #f in port) #f base-readtable)])]
      [(ch port src line col pos)
       (define next (peek-bytes 5 0 port))
       (cond [(equal? next #"ditor")
              (read-bytes 5 port)
              (read-syntax/recursive src port)]
             [else
              (define-values (in out) (make-pipe))
              (write-bytes #"#e" out)
              (close-output-port out)
              (port-count-lines! in)
              (set-port-next-location! in line col pos)
              (with-handlers ([exn:fail:read?
                               (λ (e)
                                 (raise-read-error
                                  "bad syntax"
                                  src
                                  line
                                  col
                                  pos
                                  (srcloc-span (first (exn:fail:read-srclocs e)))))])
                (read-syntax/recursive src (input-port-append #f in port) #f base-readtable))])]))
  (make-readtable base-readtable
                  #\e
                  'dispatch-macro
                  read-editor))

(module+ test
  (define (test-reader str)
    (parameterize ([current-readtable (make-editor-readtable)])
      (with-input-from-string str
        (λ ()
          (read)))))
  
  (test-reader "#e#d42.1")
  (test-reader "#d#e42.1")
  (test-reader "#editor(1 2)")
  (test-reader "(+ 1 #editor(1 2 #editor(3 4)))")
  (test-reader "(+ 1 #editor(1 #e2 3 4 5))")
  (test-reader "(+ 1 #editor(1 #e2))"))
