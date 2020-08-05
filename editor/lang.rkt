#lang racket/base

(require "private/lang.rkt"
         "private/editor.rkt"
         "base.rkt"
         racket/splicing
         (for-syntax racket/base))

(splicing-syntax-parameterize ([current-editor-lang "private/editor.rkt"]
                               [current-editor-base "base.rkt"]
                               [current-editor-modpath-mode 'package])
  (begin-for-interactive-syntax) ; <- because require happens too late...
  (require "base.rkt"
           racket/class
           (for-syntax racket/base
                       "private/lang.rkt")
           (for-editor "private/lang.rkt"
                       (from-editor "base.rkt")
                       racket/class))
  (provide (all-from-out "base.rkt")
           (for-editor (all-from-out (from-editor "base.rkt")))
           (all-from-out racket/class)
           (all-from-out racket/base)))

(module reader syntax/module-reader
  editor/lang
  #:read read
  #:read-syntax read-syntax)
