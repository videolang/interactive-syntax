#lang racket/base

(provide (all-defined-out))

(require racket/serialize
         racket/port
         racket/list
         racket/set
         syntax/srcloc
         syntax/readerr
         syntax-color/module-lexer
         "editor.rkt"
         (for-template "editor.rkt"))

(define paren-table
  (hash "(" ")"
        "{" "}"
        "[" "]"))

(define (open-paren-char? par)
  (case par
    [(\( #\( "(" \[ #\[ "[" \{ #\{ "{") #t]
    [else #f]))

(define (close-paren-char? par)
  (case par
    [(\) #\) ")" \] #\] "]" \} #\} "}") #t]
    [else #f]))

(define (close->open-paren par)
  (case par
    [(#\) ")" \)) '|(|]
    [(#\] "]" \]) '|[|]
    [(#\} "}" \}) '|{|]))

(define editor-finish "ditor")
(define editor-str (string-append "#e" "ditor"))

(define (make-editor-readtable #:readtable [base-readtable (current-readtable)]
                               #:outer-scope [user-outer-scope #f])
  (define outer-scope values);(or user-outer-scope (make-syntax-introducer #t)))
  (define (read-editor ch port src line col pos)
    (define next (peek-string (string-length editor-finish) 0 port))
    (cond [(equal? next "ditor")
           (read-string (string-length editor-finish) port)
           (define span (add1 (string-length editor-str)))
           (define next (peek-char port))
           (unless (open-paren-char? (peek-char port))
             (raise-read-error "bad syntax" src line col pos span))
           (define the-elaborator (read-syntax/recursive src port #f #f))
           (unless (open-paren-char? (peek-char port))
             (raise-read-error "bad syntax" src line col pos span))
           (define the-editor (read-syntax/recursive src port #f #f))
           (define stx (build-source-location-syntax (make-srcloc src line col pos span)))
           (define inner-scope values #;(make-syntax-introducer))
           (define-values (new-line new-col new-pos)
             (port-next-location port))
           (log-message (current-logger)
                        'info
                        'editor-lex-for-editors
                        ""
                        (vector the-elaborator
                                the-editor
                                src line col pos
                                new-line new-col new-pos))
           (outer-scope
            (inner-scope
             (datum->syntax
              the-elaborator
              `(#%editor ,the-elaborator ,the-editor)
              stx)))]
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
             (read-syntax/recursive src (input-port-append #f in port) #f base-readtable))]))
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

(define ((lex-editor base-lexer* #:fill-matches [matches #f])
         in [offset 0] [mode #f])
  (define base-lexer
    (cond
      [(not base-lexer*)
       module-lexer]
      [(procedure-arity-includes? base-lexer* 3)
       base-lexer*]
      [else (λ (in offset mode)
              (apply values
                     (append (call-with-values (λ () (base-lexer in)) list)
                             (list 0 #f))))]))
  (define-values (text type paren start end backup new-mode)
    (base-lexer in offset mode))
  (cond
    [(and (equal? text editor-str)
          (open-paren-char? (peek-char in)))
     (let loop ([cur-text text]
                [par-stack '()]
                [end end]
                [read-elaborator? #f]
                [mode new-mode])
       (define-values (text* type* p s e b n)
         (base-lexer in offset mode))
       (define new-backup
         (if (= backup 0)
             0
             (+ backup (- end start))))
       (define new-text (if (string? text*)
                            (string-append cur-text text*)
                            cur-text))
       (cond
         [(eof-object? text*)
          (values new-text 'error #f start end new-backup n)]
         [(open-paren-char? p)
          (define new-table (cons p par-stack))
          (loop new-text new-table e read-elaborator? n)]
         [(close-paren-char? p)
          (define open-par (close->open-paren p))
          (cond
            [(and (not (empty? par-stack))
                  (equal? open-par (car par-stack)))
             (define new-stack (cdr par-stack))
             (if (empty? new-stack)
                 (cond
                   [read-elaborator?
                    (when matches
                      (set-add! matches (list new-text start e)))
                    (values new-text 'parenthesis #f start e new-backup n)]
                   [else (loop new-text new-stack e #t n)])
                 (loop new-text new-stack e read-elaborator? n))]
            [else
             (values new-text 'error #f start end new-backup new-mode)])]
         [else (loop new-text par-stack e read-elaborator? n)]))]
    [else
     (values text type paren start end backup new-mode)]))

(module+ test
 (define (test-color-lexer str)
   (with-input-from-string str
     (λ ()
       (lex-editor (current-input-port)))))
  (test-color-lexer "#editor(1)(2)")
  (test-color-lexer "#editor(")
  (test-color-lexer "#editor({)"))

