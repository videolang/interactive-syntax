#lang racket/base

(provide (all-defined-out)
         (all-from-out "log.rkt")
         (for-syntax (all-defined-out)))

(require racket/class
         racket/serialize
         racket/stxparam
         racket/splicing
         racket/match
         syntax/location
         syntax/parse/define
         "log.rkt"
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
(define-syntax-parameter current-editor-lang 'editor/lang)
(define-syntax-parameter current-editor-base '(submod editor/base editor))

(define-for-syntax editor-syntax-introduce (make-syntax-introducer #t))
(define-for-syntax user-syntax-introduce (make-syntax-introducer #t))
(define-for-syntax (editor/user-syntax-introduce stx [type 'add])
  (user-syntax-introduce (editor-syntax-introduce stx type) type))

;; Creates a box for storing submodule syntax pieces.
;; Note that this box is newly instantiated for every module
;; that defines new editor types.
(begin-for-syntax
  (struct submod-data (forms
                       lifted)
    #:transparent
    #:mutable)
  (define the-submod-data (submod-data '() #f))
  (define deserializer-submod-data (submod-data '() #f))
  (define (add-syntax-to-submod! stx submod submod-name
                                 #:lang-submod [lang-submod #f]
                                 #:base-submod [base-submod #f]
                                 #:scopes [scp #f]
                                 #:required? [req? #t])
    (define existing (submod-data-forms submod))
    (when (and (not (submod-data-lifted submod)) req?)
      (syntax-local-lift-module-end-declaration
       #`(#,submod-name
          #,@(if base-submod (list base-submod) '())
          #,@(if lang-submod (list lang-submod) '())))
      (set-submod-data-lifted! submod #t))
    (set-submod-data-forms! submod (append (reverse (syntax->list stx)) existing)))
  (define (add-syntax-to-editor! stx
                                 #:scopes [scp #f]
                                 #:required? [req? #t])
    (add-syntax-to-submod! stx the-submod-data
                           #'define-interactive-syntax-submodule
                           #:base-submod (syntax-parameter-value #'current-editor-base)
                           #:lang-submod (syntax-parameter-value #'current-editor-lang)
                           #:scopes scp
                           #:required? req?))
  (define (add-syntax-to-deserializer! stx
                                       #:scopes [scp #f]
                                       #:required? [req? #t])
    (add-syntax-to-submod! stx deserializer-submod-data
                           #'define-deserializer-submodule
                           #:base-submod 'racket/base
                           #:scopes scp
                           #:required? req?)))

(define-syntax (editor-submod stx)
  (syntax-parse stx
    [(_ (~or (~optional (~seq #:required? req?:boolean) #:defaults ([req? #'#t])))
        body ...)
     (case (syntax-local-context)
       [(module)
        (add-syntax-to-editor! (syntax-local-introduce #'(body ...))
                               #:scopes (let ([_ (attribute body)])
                                          (if (pair? _) (car _) #f))
                               #:required? (syntax-e #'req?))
        #'(begin)]
       [else #`(begin #,stx)])]))

(define-syntax (deserializer-submod stx)
  (syntax-parse stx
    [(_ body ...)
     (case (syntax-local-context)
       [(module)
        (add-syntax-to-deserializer! (syntax-local-introduce #'(body ...))
                                     #:scopes (let ([_ (attribute body)])
                                                (if (pair? _) (car _) #f)))
        #'(begin)]
       [else #'(begin #,stx)])]))

(define-for-syntax (wrap-scope scopes stx)
  (datum->syntax scopes (syntax-e stx)))

(define-syntax-parser define-interactive-syntax-submodule
  [(_ base lang)
   (define base-scope
     (editor-syntax-introduce (syntax-local-introduce (datum->syntax #f #f))))
   #`(module* editor racket/base
       (require #,(wrap-scope base-scope #'base)
                #,(wrap-scope base-scope #'lang)
                racket/serialize
                racket/class)
       #,@(map syntax-local-introduce (reverse (submod-data-forms the-submod-data))))])

(define-syntax-parser define-deserializer-submodule
  [(_ base)
   (define base-scope
     (editor-syntax-introduce (syntax-local-introduce (datum->syntax #f #f))))
   #`(module* deserializer racket/base
       (require #,(wrap-scope base-scope #'base)
                racket/serialize
                racket/class)
       #,@(map syntax-local-introduce (reverse (submod-data-forms deserializer-submod-data))))])

;; ===================================================================================================

;; Expand for-editor to a recognized module path
;; editor-module-path? -> module-path?
(define-for-syntax (expand-editor-req-path path)
  (match path
    [`(from-editor ',mod)
     `(submod ".." mod editor)]
    [`(from-editor (submod ".." ,subpath ...))
     `(submod ".." ".." ,@subpath editor)]
    [`(from-editor (submod "." ,subpath ...))
     `(submod ".." ,@subpath editor)]
    [`(from-editor (submod ,subpath ...))
     `(submod ,@subpath editor)]
    [`(from-editor ,mod)
     `(submod ,mod editor)]
    [_ path]))

;; Test to see if the given submodule exists.
;; If it does, then require it, otherwise `(begin)`.
;; Must only be used at top/module level.
(define-syntax-parser maybe-require-submod
  [(_ phase mod-path)
   (define expanded-modpath (expand-editor-req-path `(from-editor ,(syntax->datum #'mod-path))))
   (when (or (with-handlers* ([exn:fail? (λ (e) #f)])
               (module-declared?
                (convert-relative-module-path expanded-modpath)
                #t))
             (with-handlers* ([exn:fail? (λ (e) #f)])
               (expand-import #'(from-editor mod-path))
               #t))
     (define expanded-modpath-stx (datum->syntax #'#f expanded-modpath))
     (define scopes (editor/user-syntax-introduce #'mod-path))
     (add-syntax-to-editor!
      (syntax-local-introduce
       #`((~require (for-meta phase #,(wrap-scope scopes expanded-modpath-stx)))))
       #:required? #f))
   #'(begin)])

;; We want to require edit-time code into the modules editor submod.
(define-syntax (~require stx)
  ;(printf "req:~s~n" stx)
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
     ;(printf "mreq:~s~n" #'(maybe-reqs ...))
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
                                      (editor/user-syntax-introduce (pre-expand-export i mode) 'add))
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
           #:with (marked-name ...) (editor/user-syntax-introduce #'(name ...) 'add)
           #:with r/b (editor-syntax-introduce
                       (datum->syntax stx (syntax-parameter-value #'current-editor-lang)))
           (add-syntax-to-editor! (syntax-local-introduce #'((require r/b marked-name ...))))
           (values '() '())])))
    #:property prop:provide-pre-transformer
    (λ (str)
      (λ (stx mode)
        (syntax-parse stx
          [(_ name ...)
           #:with (marked-name ...) (editor/user-syntax-introduce #'(name ...) 'add)
           ;(printf "for-editor: ~s~n" stx)
           (add-syntax-to-editor! (syntax-local-introduce #'((provide marked-name ...))))
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

(define-for-syntax (expand-editorpath path)
  (syntax-parse path
    #:literals (from-editor submod)
    [(from-editor (submod subpath ...))
     #'(submod subpath ... editor)]
    [(from-editor mod)
     #'(submod mod editor)]
    [_ path]))

;; Just as for-editor is similar to for-syntax, for-elaborator
;; is similar to for-template. It lets helper modules bring in
;; editor components from another module.
(begin-for-syntax
  (struct from-editor-struct ()
    #:property prop:procedure
    (λ (f stx)
      (syntax-parse stx
        [(_ mod)
         #'(let ([m mod])
             (match m
               [`(submod ,x ,rest (... ...)) `(submod ,x ,@rest editor)]
               [x `(submod ,x editor)]))]))
    #:property prop:require-transformer
    (λ (str)
      (λ (stx)
        (syntax-parse stx
          [(_ name ...)
           (for/fold ([i-list '()]
                      [is-list '()])
                     ([n (in-list (attribute name))])
             ;; XXX This NEEDS a proper from-editor implementation.
             (define-values (imports is)
               (expand-import (expand-editorpath #`(from-editor #,n))))
             (define new-imports
               (for/list ([i (in-list imports)])
                 (struct-copy import i
                              [local-id (format-id n "~a" (import-local-id i))])))
             (values (append new-imports i-list)
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

(define-syntax (begin-for-interactive-syntax stx)
  (syntax-parse stx
    [(_ code ...)
     #:with (marked-code ...) (editor/user-syntax-introduce #'(code ...))
     (syntax/loc stx
       (editor-submod
        marked-code ...))]))

(define-syntax (define-for-interactive-syntax stx)
  (syntax-parse stx
    [(_ name:id body)
     (syntax/loc stx
       (begin-for-interactive-syntax
         (define name body)))]
    [(_ name:function-header body)
     (syntax/loc stx
       (begin-for-interactive-syntax
         (define name body)))]))

