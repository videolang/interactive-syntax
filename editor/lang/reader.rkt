#lang s-exp syntax/module-reader
editor
#:wrapper1 (λ (t)
             (parameterize ([current-readtable (make-editor-readtable)])
               (t)))
#:info (λ (key default defproc)
         (case key
           [(color-lexer) (λ (in)
                            (define-values (text type paren start end)
                              (racket-lexer in))
                            (cond
                              [(equal? text "#editor")
                               (values text 'parenthesis paren start end)]
                              [else
                               (values text type paren start end)]))]
           [else (defproc key default)]))

(require syntax-color/racket-lexer
         "read-editor.rkt")
