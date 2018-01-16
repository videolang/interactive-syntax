#lang s-exp syntax/module-reader
editor
#:wrapper1 (λ (t)
             (parameterize ([current-readtable (make-editor-readtable)])
               (t)))
#:info (λ (key default defproc)
         (case key
           [(color-lexer) lex-editor]
           [(definitions-text-surrogate) 'editor/lang/surrogate]
           [(drracket:toolbar-buttons)
            (define toggle-button
              (dynamic-require 'editor/lang/surrogate 'toggle-button))
            (list toggle-button)]
           [else (defproc key default)]))

(require "read-editor.rkt")
