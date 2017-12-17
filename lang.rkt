#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/serialize
         racket/stxparam
         racket/splicing
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define serial-key (generate-member-key))
(define deserial-key (generate-member-key))
(define copy-key (generate-member-key))

(begin-for-syntax
  (define-syntax-class defstate
    #:literals (define-state)
    (pattern (define-state name body ...)))
  (define-syntax-class defpubstate
    #:literals (define-public-state)
    (pattern (define-public-state name body ...))))

(define-syntax-parameter defstate-parameter
  (syntax-parser
    [(_ stx who)
     (raise-syntax-error #'who "Use outside of define-editor is an error" this-syntax)]))

(define-syntax (define-state stx)
  (syntax-parse stx
    [x:defstate
     #`(defstate-parameter #,stx define-state)]))

(define-syntax (define-public-state stx)
  (syntax-parse stx
    [x:defpubstate
     #`(defstate-parameter #,stx define-public-state)]))

(define-syntax (~define-editor stx)
  (syntax-parse stx
    [(_ orig-stx name:id supclss (interfaces ...)
        (~or (~optional (~seq #:base? b?) #:defaults ([b? #'#f]))
             (~optional (~seq #:direct-deserialize? dd?) #:defaults ([dd? #'#t])))
        ...
        (~and
         (~seq (~or state:defstate public-state:defpubstate internal-body) ...)
         (~seq body ...)))
     #:with name-deserialize (format-id stx "~a:deserialize" #'name)
     (define dd?* (syntax-e #'dd?))
     (unless (or (not dd?*) (eq? 'module (syntax-local-context)))
       (raise-syntax-error #f "Must be defined at the module level" #'orig-stx))
     (define serialize-method (gensym 'serialize))
     (define deserialize-method (gensym 'deserialize))
     (define copy-method (gensym 'copy))
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
         (splicing-syntax-parameterize ([defstate-parameter
                                          (syntax-parser
                                            [(_ st:defstate who)
                                             #'(define st.name st.body (... ...))]
                                            [(_ st:defpubstate who)
                                             #'(field [st.name st.body (... ...)])])])
           (define name
             (let ()
               #,@(for/list ([i (in-list (attribute state.name))])
                    #`(define-local-member-name #,i))
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
                  #,@(for/list ([i (in-list (attribute state.name))])
                       #`(set! #,i (get-field #,i other)))
                  #,@(for/list ([i (in-list (attribute public-state.name))])
                       #`(set! #,i (get-field #,i other)))
                  (void))
                (#,(if base? #'public #'override) #,copy-method)
                body ...)))))]))

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
