#lang racket/base

(module reader racket/base

  (provide (rename-out [e:read read]
                       [e:read-syntax read-syntax]
                       [e:get-info get-info]))
  (require syntax/module-reader
           syntax/parse
           "private/read-editor.rkt")

  (define ((wrap-reader t) . args)
    (define outer-scope (make-syntax-introducer #t))
    (parameterize ([current-readtable (make-editor-readtable #:outer-scope outer-scope)])
      (define stx (apply t args))
      (if (syntax? stx)
          (syntax-parse stx
            [(module name lang (mod-beg body ...))
             (outer-scope
              #`(module name lang
                  (mod-beg
                   #,(outer-scope #'(#%require (only editor/base)))
                   body ...)))])
          stx)))

  (define-values (e:read e:read-syntax e:get-info)
    (make-meta-reader
     'editor
     "Embedded Editors"
     lang-reader-module-paths
     wrap-reader
     wrap-reader
     (λ (defproc)
       (λ (key default)
         (case key
           [(color-lexer) lex-editor]
           [(definitions-text-surrogate) 'editor/private/surrogate]
           [(drracket:toolbar-buttons)
            (define toggle-button
              (dynamic-require 'editor/private/surrogate 'toggle-button))
            (list toggle-button)]
           [else (defproc key default)]))))))
