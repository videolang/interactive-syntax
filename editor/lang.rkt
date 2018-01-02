#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/serialize
         racket/stxparam
         racket/splicing
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/lib/function-header
                     racket/serialize))

;; Only introduced by #editor reader macro. Handles deserializing
;;  the editor.
(define-syntax (#%editor stx)
  (syntax-parse stx
    [(_ elaborator body)
     #'(elaborator body)]))

(define serial-key (generate-member-key))
(define deserial-key (generate-member-key))
(define copy-key (generate-member-key))

(begin-for-syntax
  (define-syntax-class defelaborate
    #:literals (define-elaborate)
    (pattern (define-elaborate data body ...+)))
  (define-syntax-class defstate
    #:literals (define-state)
    (pattern (define-state name body ...)
             #:attr getter (format-id this-syntax "get-~a" #'name)
             #:attr setter (format-id this-syntax "set-~a!" #'name)))
  (define-syntax-class defpubstate
    #:literals (define-public-state)
    (pattern (define-public-state name body ...))))

(define-syntax-parameter defstate-parameter
  (syntax-parser
    [(_ stx who)
     (raise-syntax-error #'who "Use outside of define-editor is an error" this-syntax)]))

(define-syntax define-elaborate
  (syntax-parser
    [de:defelaborate
     (raise-syntax-error this-syntax "Use outside of define-editor is an error" this-syntax)]))

(define-syntax (define-state stx)
  (syntax-parse stx
    [x:defstate
     #`(defstate-parameter #,stx define-state)]))

(define-syntax (define-public-state stx)
  (syntax-parse stx
    [x:defpubstate
     #`(defstate-parameter #,stx define-public-state)]))

;; Each editor definition has three parts:
;; 1. A phase 1 elaboration
;; 2. A submodule with interaction code
;; 3. A deserializer submodule
(define-syntax (~define-editor stx)
  (syntax-parse stx
    [(_ orig-stx name:id supclss (interfaces ...)
        (~or (~optional (~seq #:base? b?) #:defaults ([b? #'#f]))
             (~optional (~seq #:direct-deserialize? dd?) #:defaults ([dd? #'#t])))
        ...
        (~and
         (~seq (~or state:defstate
                    public-state:defpubstate
                    (~optional elaborator:defelaborate
                               #:defaults ([elaborator.data #'this]
                                           [(elaborator.body 1) (list #'#'this)]))
                    internal-body) ...)
         (~seq body ...)))
     #:with name-deserialize (format-id stx "~a:deserialize" #'name)
     #:with editor-submod (format-id stx "editor-info")
     (define dd?* (syntax-e #'dd?))
     (unless (or (not dd?*) (eq? 'module (syntax-local-context)))
       (raise-syntax-error #f "Must be defined at the module level" #'orig-stx))
     (define serialize-method (gensym 'serialize))
     (define deserialize-method (gensym 'deserialize))
     (define copy-method (gensym 'copy))
     (define state-methods (for/list ([i (in-list (attribute state.getter))])
                             (gensym (syntax->datum i))))
     (define base? (syntax-e (attribute b?)))
     #`(begin
         (define-member-name #,serialize-method serial-key)
         (define-member-name #,deserialize-method deserial-key)
         (define-member-name #,copy-method copy-key)
         #,@(if dd?*
                (list
                 #`(provide name-deserialize)
                 #`(define name-deserialize
                     (make-deserialize-info
                      (λ (sup table public-table)
                        (define this (new name))
                        (send this #,deserialize-method (vector sup table public-table))
                        this)
                      (λ ()
                        (define pattern (new name))
                        (values pattern
                                (λ (other)
                                  (send pattern #,copy-method other)))))))
                '())
         (define-syntax (elaborator.name stx)
           (syntax-parse stx
             [(_ elaborator.data)
              elaborator.body ...]))
         (splicing-syntax-parameterize ([defstate-parameter
                                          (syntax-parser
                                            [(_ st:defstate who)
                                             #'(begin
                                                 (define st.name st.body (... ...)))]
                                            [(_ st:defpubstate who)
                                             #'(field [st.name st.body (... ...)])])])
           ;(module+ editor-submod
             (define name
               (let ()
                 #,@(for/list ([sm (in-list state-methods)])
                      #`(define-local-member-name #,sm))
                 (class/derived
                  orig-stx
                  (name
                   supclss
                   ((interface* () ([prop:serializable
                                     (make-serialize-info
                                      (λ (this)
                                        (send this #,serialize-method))
                                      #'name-deserialize
                                      #t
                                      (or (current-load-relative-directory) (current-directory)))]))
                    interfaces ...)
                   #f)
                  (define (#,serialize-method)
                    (vector #,(if base?
                                  #'#f
                                  #`(super #,serialize-method))
                            (make-immutable-hash
                             `#,(for/list ([i (in-list (attribute state.name))])
                                  #`(#,(syntax->datum i) . ,#,i)))
                            (make-immutable-hash
                             `#,(for/list ([i (in-list (attribute public-state.name))])
                                  #`(#,(syntax->datum i) . ,#,i)))))
                  (#,(if base? #'public #'override) #,serialize-method)
                  (define (#,deserialize-method data)
                    (define sup (vector-ref data 0))
                    (define table (vector-ref data 1))
                    (define public-table (vector-ref data 2))
                    #,(if base?
                          #`(void)
                          #`(super #,deserialize-method sup))
                    #,@(for/list ([i (in-list (attribute state.name))])
                         #`(set! #,i (hash-ref table '#,(syntax->datum i))))
                    #,@(for/list ([i (in-list (attribute public-state.name))])
                         #`(set! #,i (hash-ref public-table '#,(syntax->datum i)))))
                  (#,(if base? #'public #'override) #,deserialize-method)
                  (define (#,copy-method other)
                    #,(if base?
                          #`(void)
                          #`(super #,copy-method other))
                    #,@(for/list ([i (in-list (attribute state.name))]
                                  [get (in-list state-methods)])
                         #`(set! #,i (send other #,get)))
                    #,@(for/list ([i (in-list (attribute public-state.name))])
                         #`(set! #,i (get-field #,i other)))
                    (void))
                  (#,(if base? #'public #'override) #,copy-method)
                  #,@(for/list ([i (in-list (attribute state.name))]
                                [sm (in-list state-methods)])
                       #`(define/public (#,sm) #,i))
                  body ...)))));)
     ]))

(define-syntax (define-base-editor* stx)
  (syntax-parse stx
    [(_ name:id super (interfaces ...) body ...)
     #`(~define-editor #,stx name super (interfaces ...) #:base? #t body ...)]))

(define-syntax (define-editor stx)
  (syntax-parse stx
    [(_ name:id super
        (~or (~optional (~seq #:interfaces (interfaces ...)) #:defaults ([(interfaces 1) '()])))
        ...
        body ...)
     #`(~define-editor #,stx name super (interfaces ...) body ...)]))

(define-syntax (define-editor-mixin stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~optional (~seq #:interfaces (interfaces ...)) #:defaults ([(interfaces 1) '()]))
             (~optional (~seq #:mixins (mixins ...)) #:defaults ([(mixins 1) '()])))
        ...
        body ...)
     #`(begin
         (define (name $)
           (~define-editor #,stx
                           name
                           ((compose #,@(reverse (attribute mixins))) $)
                           (interfaces ...)
                           #:direct-deserialize? #f
                           body ...)
           name))]))

(define-logger editor)
