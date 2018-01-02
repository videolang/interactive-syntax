#lang racket/base

(provide (all-defined-out))

(require racket/serialize
         racket/port
         racket/list
         syntax/srcloc
         syntax/readerr
         (for-template "../lang.rkt"))

(define (paren-char? par)
  (case par
    [(#\( #\[ #\{) #t]
    [else #f]))

(define (make-editor-readtable #:readtable [base-readtable (current-readtable)])
  (define read-editor
    (λ (ch port src line col pos)
      (define next (peek-bytes 5 0 port))
      (cond [(equal? next #"ditor")
             (read-bytes 5 port)
             (define span 8)
             (define next (peek-char port))
             (unless (paren-char? (peek-char port))
               (raise-read-error "bad syntax" src line col pos span))
             (define the-elaborator (read-syntax/recursive src port))
             (unless (paren-char? (peek-char port))
               (raise-read-error "bad syntax" src line col pos span))
             (define the-editor (read-syntax/recursive src port))
             (define stx (build-source-location-syntax (make-srcloc src line col pos span)))
             (quasisyntax/loc stx
               (#%editor #,the-elaborator #,the-editor))]
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
               (read-syntax/recursive src (input-port-append #f in port) #f base-readtable))])))
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
