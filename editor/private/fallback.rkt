#lang racket/base

(require racket/splicing
         "editor.rkt"
         (for-syntax racket/base))

(splicing-syntax-parameterize ([current-editor-lang "../lang.rkt"]
                               [current-editor-base '(submod "../base.rkt" editor)]
                               [current-editor-modpath-mode 'package])
  (require "stdlib.rkt")
  
  (define-editor fallback$ base$
    (super-new)))
