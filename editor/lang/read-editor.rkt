#lang racket/base

(provide (all-defined-out))

(require racket/serialize
         racket/port
         racket/list
         racket/set
         syntax/srcloc
         syntax/readerr
         syntax-color/racket-lexer
         (for-template "../lang.rkt"))

(define paren-table
  (hash "(" ")"
        "{" "}"
        "[" "]"))

(define (open-paren-char? par)
  (case par
    [(#\( "(" #\[ "[" #\{ "{") #t]
    [else #f]))

(define (close-paren-char? par)
  (case par
    [(#\) ")" #\] "]" #\} "}") #t]
    [else #f]))

(define (close->open-paren par)
  (case par
    [(#\) ")") "("]
    [(#\] "]") "["]
    [(#\} "}") "{"]))

(define editor-finish "ditor")
(define editor-str (string-append "#e" "ditor"))

(define (make-editor-readtable #:readtable [base-readtable (current-readtable)])
  (define read-editor
    (λ (ch port src line col pos)
      (define next (peek-string (string-length editor-finish) 0 port))
      (cond [(equal? next "ditor")
             (read-string (string-length editor-finish) port)
             (define span (add1 (string-length editor-str)))
             (define next (peek-char port))
             (unless (open-paren-char? (peek-char port))
               (raise-read-error "bad syntax" src line col pos span))
             (define the-elaborator (read-syntax/recursive src port))
             (unless (open-paren-char? (peek-char port))
               (raise-read-error "bad syntax" src line col pos span))
             (define the-editor (read-syntax/recursive src port))
             (define stx (build-source-location-syntax (make-srcloc src line col pos span)))
             (quasisyntax/loc stx
               (#%editor #,the-elaborator #,the-editor))]
            [else
             (define-values (in out) (make-pipe))
             (write-string "#e" out)
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
  (test-reader "#editor(1 2)()")
  (test-reader "(+ 1 #editor(1 2 #editor(3 4)())())")
  (test-reader "(+ 1 #editor(1 #e2 3 4 5)())")
  (test-reader "(+ 1 #editor(1 #e2)())"))

(define (lex-editor in
                    #:fill-matches [matches #f]) ; offset mode*)
  ;(define mode (or mode* '()))
  (define-values (text type paren start end)
    (racket-lexer in))
  (cond
    [(and (equal? text editor-str)
          (open-paren-char? (peek-char in)))
     (let loop ([cur-text text]
                [par-stack '()]
                [end end]
                [read-elaborator? #f])
       (define-values (text* type* p s e)
         (racket-lexer in))
       (define new-text (if (string? text*)
                            (string-append cur-text text*)
                            cur-text))
       (cond
         [(eof-object? text*)
          (values new-text 'error #f start end)]
         [(open-paren-char? text*)
          (define new-table (cons text* par-stack))
          (loop new-text new-table e read-elaborator?)]
         [(close-paren-char? text*)
          (define open-par (close->open-paren text*))
          (cond
            [(and (not (empty? par-stack))
                  (equal? open-par (car par-stack)))
             (define new-stack (cdr par-stack))
             (if (empty? new-stack)
                 (cond
                   [read-elaborator?
                    (when matches
                      (set-add! matches (list new-text start e)))
                    (values new-text 'parenthesis #f start e)]
                   [else (loop new-text new-stack e #t)])
                 (loop new-text new-stack e read-elaborator?))]
            [else
             (values new-text 'error #f start end)])]
         [else (loop new-text par-stack e read-elaborator?)]))]
    [else
     (values text type paren start end)]))

(module+ test
 (define (test-color-lexer str)
   (with-input-from-string str
     (λ ()
       (lex-editor (current-input-port)))))
  (test-color-lexer "#editor(1)(2)")
  (test-color-lexer "#editor(")
  (test-color-lexer "#editor({)"))

