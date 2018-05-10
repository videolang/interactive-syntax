#lang racket/base

(require "base.rkt"
         "private/lang.rkt"
         racket/splicing
         (for-syntax racket/base))

(splicing-syntax-parameterize ([current-editor-lang "private/editor.rkt"])
  (begin-for-editor) ; <- because require happens too late...
  (require (for-syntax racket/base
                       "private/lang.rkt")
           (for-editor "private/lang.rkt"
                       (from-editor "base.rkt")))
  
  (provide (all-from-out "base.rkt")
           (for-editor (~all-from-out (from-editor "base.rkt")))
           (all-from-out racket/base)))
