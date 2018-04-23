#lang racket/base

(provide (rename-out [e:read read]
                     [e:read-syntax read-syntax]
                     [e:get-info get-info]))
(require syntax/module-reader)

(define ((wrap-reader t) . args)
  (parameterize ([current-readtable (make-editor-readtable)])
    (apply t args)))

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
         [(definitions-text-surrogate) 'editor/lang/surrogate]
         [(drracket:toolbar-buttons)
          (define toggle-button
            (dynamic-require 'editor/lang/surrogate 'toggle-button))
          (list toggle-button)]
         [else (defproc key default)])))))

(require "read-editor.rkt")
