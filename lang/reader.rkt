#lang s-exp syntax/module-reader
racket
#:wrapper1 (λ (t)
             (parameterize ([current-readtable (make-editor-readtable)])
               (t)))

(require "read-editor.rkt")
