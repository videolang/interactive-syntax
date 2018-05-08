#lang racket/base

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(require racket/class
         racket/serialize
         racket/stxparam
         racket/splicing
         racket/match
         syntax/location
         syntax/parse/define
         (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/function
                     racket/require-transform
                     racket/provide-transform
                     racket/syntax
                     syntax/parse
                     syntax/parse/lib/function-header
                     syntax/location
                     racket/serialize))

;; To be able to instantiate the found editors, we need each
;; module to be able to track the editors created in its
;; (partially defined) file.
(module key-submod racket/base
  ;(#%declare #:cross-phase-persistent)
  (provide editor-list-key editor-mixin-list-key)
  (define editor-list-key 'editor-list-cmark-key)
  (define editor-mixin-list-key 'editor-mixin-list-cmark-key))
(require (for-syntax 'key-submod))

;; ===================================================================================================

;; Because we use lang in building the stdlib, which is exported
;; as part of the lang, we want to use racket/base to bootstrap
;; that language.
(define-for-syntax current-editor-lang (make-parameter 'editor/lang))
(define-for-syntax current-editor-base (make-parameter 'editor/base))

(define-for-syntax editor-syntax-introduce (make-syntax-introducer))

;; Creates a box for storing submodule syntax pieces.
;; Note that this box is newly instantiated for every module
;; that defines new editor types.
(begin-for-syntax
  (struct submod-data (required
                       maybe)
    #:transparent
    #:mutable)
  (define the-submod-data (submod-data '() '()))
  (define (add-syntax-to-editor! stx
                                 #:required? [req? #t])
    (define existing ((if req? submod-data-required submod-data-maybe)
                      the-submod-data))
    (when (null? (submod-data-required the-submod-data))
      (syntax-local-lift-module-end-declaration
       #'(define-editor-submodule)))
    ((if req? set-submod-data-required! set-submod-data-maybe!)
     the-submod-data (append (reverse (syntax->list stx)) existing))))

(define-syntax (editor-submod stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:required? req?:boolean) #:defaults ([req? #'#t]))
        body ...)
     (add-syntax-to-editor! (syntax-local-introduce #'(body ...))
                            #:required? (syntax-e #'req?))
     #'(begin)]))

(define-syntax (define-editor-submodule stx)
  (syntax-parse stx
    [(_)
     #`(module* editor racket/base
         (require racket/serialize
                  racket/class)
         #,@(map syntax-local-introduce (reverse (submod-data-required the-submod-data)))
         #,@(map syntax-local-introduce (reverse (submod-data-required the-submod-data))))]))

;; ===================================================================================================

;; Expand for-editor to a recognized module path
;; editor-module-path? -> module-path?
(define-for-syntax (expand-editorpath path)
  (match path
    [`(for-editor (submod ,subpath ...))
     `(submod ,@subpath editor)]
    [`(for-editor ,mod)
     `(submod ,mod editor)]
    [_ path]))

;; Test to see if the given submodule exists.
;; If it does, then require it, otherwise `(begin)`.
;; Must only be used at top/module level.
(define-syntax-parser maybe-require-submod
  [(_ phase mod-path)
   (if (module-declared?
        (convert-relative-module-path (expand-editorpath `(for-editor ,(syntax->datum #'mod-path))))
        #t)
       #'(~require (for-editor (for-meta phase (from-editor mod-path))))
       #'(begin))])

;; We want to require edit-time code into the modules editor submod.
(define-syntax (~require stx)
  (syntax-parse stx
    [(_ body ...)
     (define/syntax-parse (maybe-reqs ...)
       (append*
        (for/list ([i (in-list (attribute body))])
          (define-values (imports import-sources) (expand-import i))
          (for/list ([s (in-list import-sources)])
            (match-define (struct* import-source ([mod-path-stx mod-path]
                                                  [mode phase]))
              s)
            #`(maybe-require-submod #,phase #,mod-path)))))
     #'(begin (require body ...)
              maybe-reqs ...)]))

;; We also want all-from-out to respect `from-editor`.
(define-syntax ~all-from-out
  (make-provide-pre-transformer
   (λ (stx mode)
     ;(printf "afo-pre: ~s~n" stx)
     (syntax-parse stx
       [(_ paths ...)
        #:with (expanded-paths ...) (for/list ([i (in-list (attribute paths))])
                                      (pre-expand-export i mode))
        ;(printf "afo-post: ~s~n" #'(expanded-paths ...))
        #'(all-from-out expanded-paths ...)]))))

(define-syntax provide-key #'provide-key)

;; Since the editor submodule is a language detail, we want
;; a dedicated for-editor require subform.
(begin-for-syntax
  (struct for-editor-struct ()
    #:property prop:require-transformer
    (λ (str)
      (λ (stx)
        (syntax-parse stx
          [(_ name ...)
           #:with (marked-name ...) (editor-syntax-introduce #'(name ...) 'add)
           #:with r/b (editor-syntax-introduce (format-id stx "racket/base"))
           (syntax-local-lift-module-end-declaration
            #`(editor-submod
               (require r/b marked-name ...)))])
        (values '() '())))
    #:property prop:provide-pre-transformer
    (λ (str)
      (λ (stx mode)
        (syntax-parse stx
          [(_ name ...)
           #:with (marked-name ...) (editor-syntax-introduce #'(name ...) 'add)
           ;(printf "for-editor: ~s~n" stx)
           (syntax-local-lift-module-end-declaration
            #`(editor-submod
               (provide marked-name ...)))
           #'(for-editor provide-key name ...)])))
    #:property prop:provide-transformer
    (λ (str)
      (λ (stx mode)
        (syntax-parse stx
          [(_ (~literal provide-key) name ...)
           '()]
          [else
           (raise-syntax-error 'for-editor "Not a provide sub-form" stx)])))))

(define-syntax for-editor (for-editor-struct))

;; Just as for-editor is similar to for-syntax, for-elaborator
;; is similar to for-template. It lets helper modules bring in
;; editor components from another module.
(begin-for-syntax
  (struct from-editor-struct ()
    #:property prop:require-transformer
    (λ (str)
      (λ (stx)
        (syntax-parse stx
          [(_ name ...)
           (for/fold ([i-list '()]
                      [is-list '()])
                     ([n (in-list (attribute name))])
             ;; XXX This NEEDS a proper from-editor implementation.
             (define-values (i is)
               (expand-import (datum->syntax stx `(submod ,n editor))))
             (values (append i i-list)
                     (append is is-list)))])))
    #:property prop:provide-pre-transformer
    (λ (str)
      (λ (stx mode)
        (syntax-parse stx
          [(_ name)
           ;(printf "from-editor: ~s~n" stx)
           (datum->syntax stx `(submod ,#'name editor))]
          [(_ name ...)
           #:with (subnames ...) (for/list ([i (in-list (attribute name))])
                                   (datum->syntax stx `(submod i editor)))
           #`(combine-out subnames ...)])))))

(define-syntax from-editor (from-editor-struct))

(define-syntax (begin-for-editor stx)
  (syntax-parse stx
    [(_ code ...)
     #:with baselang (editor-syntax-introduce (datum->syntax stx (current-editor-lang)))
     #:with (marked-code ...) (editor-syntax-introduce #'(code ...))
     (syntax/loc stx
       (editor-submod
        (require baselang)
        marked-code ...))]))

(define-syntax (define-for-editor stx)
  (syntax-parse stx
    [(_ name:id body)
     (syntax/loc stx
       (begin-for-editor
         (define name body)))]
    [(_ name:function-header body)
     (syntax/loc stx
       (begin-for-editor
         (define name body)))]))

(define-logger editor)
