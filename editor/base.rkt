#lang racket/base

(require "private/lang.rkt"
         racket/splicing
         (for-syntax racket/base))

(splicing-syntax-parameterize ([current-editor-lang 'racket/base]
                               [current-editor-base "private/editor.rkt"])
  (begin-for-editor) ; <- because require happens too late...
  (require
    "private/editor.rkt"
    "private/stdlib.rkt"
    (for-editor "private/lang.rkt"
                (from-editor "private/stdlib.rkt")))

  (provide (~all-from-out "private/stdlib.rkt")
           (for-editor (~all-from-out (from-editor "private/stdlib.rkt")))
           define-editor
           define-editor-mixin
           begin-for-editor
           define-for-editor
           for-editor
           from-editor
           (rename-out [~require require]
                       [~all-from-out all-from-out])))
