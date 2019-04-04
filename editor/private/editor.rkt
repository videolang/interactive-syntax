#lang racket/base

(require "lang.rkt"
         racket/list
         racket/stxparam
         racket/class
         racket/splicing
         syntax/location
         racket/serialize
         racket/dict
         syntax/parse/define
         racket/runtime-path
         racket/match
         racket/stxparam
         (for-syntax racket/base
                     racket/class
                     (submod "lang.rkt" key-submod)
                     racket/match
                     racket/serialize
                     racket/function
                     syntax/location
                     racket/syntax
                     racket/path
                     racket/pretty
                     syntax/parse))
(provide (all-defined-out)
         (all-from-out "lang.rkt")
         (for-syntax all-defined-out))

(define EDITOR-SERIALIZE-VERSION 0)

;; Keys for hidden methods. Only functions in
;;   this module should access these methods directly.
(define serial-key (generate-member-key))
(define deserial-key (generate-member-key))
(define deserial-binding-key (member-name-key deserialize-binding))
(define copy-key (generate-member-key))
(define elaborator-key (member-name-key elaborate))
(define modpath-key (member-name-key get-modpath))

(define editor-deserialize-for-elaborator
  (make-parameter #f))

(module m->r racket/base
  (provide (all-defined-out))
  (require racket/path
           racket/match
           syntax/parse/define
           syntax/modresolve
           syntax/location
           (for-syntax racket/base))
  (define (modpath->relpath modpath)
    (if (path-string? modpath)
        (path-only modpath)
        modpath))
  (define-syntax (this-mod-dir stx)
    (syntax/loc stx
      (modpath->relpath (resolve-module-path-index
                         (module-path-index-join "here.rkt" #f))))))
(require 'm->r (for-syntax 'm->r))

(define-syntax-parameter current-editor-modpath-mode 'user)
(begin-for-syntax
  (define (package/quote-module-path)
    (case (syntax-parameter-value #'current-editor-modpath-mode)
      [(user) 'editor/private/editor]
      [(package) (quote-module-path)])))

;; Because deserialized editors use a pseudo-identifier
;;   to resolve to an elaborator, we need to reconstruct a
;;   racket identifier out of their symbol and modpath.
;; NOTE THIS DOES NOT ACTUALLY REQUIRE THE MODPATH, USE
;;   syntax-local-lift-require FOR THAT!!!!  (Rather, this uses
;;   whatever module exists in the registry under that name.)
;; resolved-module-path? symbol? -> identifier?
(define-for-syntax (forge-identifier modpath sym)
  (syntax-binding-set->syntax
   (syntax-binding-set-extend (syntax-binding-set) sym 0 modpath)
   sym))

;; Only introduced by #editor reader macro. Handles deserializing
;;  the editor.
(define-syntax-parser #%editor
  [(_ binding-information body)
   #`(splicing-let-syntax
         ([this (λ (stx)
                  (parameterize ([current-load-relative-directory (this-mod-dir)])
                    (match-define `((,editor-binding ,editor-name)
                                    (,deserialize-binding ,deserialize-name)
                                    (,elaborator-binding ,elaborator-name))
                      (deserialize 'binding-information))
                    (define/syntax-parse elaborator
                      (forge-identifier
                       ;syntax-local-lift-require
                       elaborator-binding
                       elaborator-name))
                    (define/syntax-parse that-syntax (syntax-local-introduce #'#,this-syntax))
                    #'(elaborator body that-syntax)))])
       (this))])

;; Returns an identifier that contains
;;   the binding for an editor's elaborator.
;; To be put into the #editor()() form.
;; (is-a?/c editor$) -> (listof module-path-index? symbol?)
(define (editor->elaborator editor)
  (define-member-name elaborator elaborator-key)
  (define-member-name deserialize-binding deserial-binding-key)
  (define-member-name modpath modpath-key)
  (define binding ((send editor deserialize-binding)))
  (list (send editor modpath)
        (if (pair? binding)
            (list (cdr binding) (car binding))
            (list #"???" #"???"))
        (send editor elaborator)))

;; Deserializes an editor, but giving a new modpath for its
;;   deserialize id.
;; An optional dictionary of children can be provided to rehome
;;   child elements in the editor as well.
;; editor : An editor instance
;; new-modpath : Any value given to prop:serialize's deserialize-id
;; children : (Dictof <Editor> <Modpath>)
(define (serialize+rehome editor new-modpath
                          #:deserialize-relative-directory [rel-to #f]
                          #:children [children* #f])
  (define-member-name deserial-binding deserial-binding-key)
  (define children (or children* (hash)))
  (let loop ([child# (dict-iterate-first children)])
    (if child#
        (parameterize ([(send (dict-iterate-key children child#) deserial-binding)
                        (dict-iterate-value children child#)])
          (loop (dict-iterate-next children child#)))
        (parameterize ([(send editor deserial-binding) new-modpath])
          (serialize editor #:deserialize-relative-directory rel-to)))))

(define-syntax-parser define-init
  [(_ name:id default #f)
   (syntax/loc this-syntax
     (define name default))]
  [(_ name:id default #t)
   (syntax/loc this-syntax
     (define-init name default (λ (x) x)))]
  [(_ name:id default init-proc)
   (quasisyntax/loc this-syntax
     (begin
       (init [(ist name) default])
       #,(syntax/loc this-syntax
           (define name (init-proc ist)))))])


(define-syntax-parser define-getter
  [(_ _:id _:id #f)
   (syntax/loc this-syntax (void))]
  [(_ state:id getter:id #t)
   (quasisyntax/loc this-syntax
     (define-getter state getter #,(syntax/loc this-syntax (λ () state))))]
  [(_ _:id getter:id body)
   (syntax/loc this-syntax (define/public getter body))])

(define-syntax-parser define-setter
  [(_ _:id _:id #f)
   (syntax/loc this-syntax (void))]
  [(_ state:id setter:id #t)
   (quasisyntax/loc this-syntax
     (define-setter state setter #,(syntax/loc this-syntax (λ (new-val) (set! state new-val)))))]
  [(_ _:id setter:id body)
   (syntax/loc this-syntax (define/public setter body))])

(begin-for-syntax
  (define-syntax-class defelaborate
    #:literals (define-elaborate)
    (pattern (define-elaborate data
               (~optional (~seq #:this-editor this-editor))
               body ...+)))
  (define-splicing-syntax-class defstate-options
    (pattern (~seq
              (~alt (~optional (~seq #:persistence persistence) #:defaults ([persistence #'#t]))
                    (~optional (~seq #:getter getter) #:defaults ([getter #'#f]))
                    (~optional (~seq #:setter setter) #:defaults ([setter #'#f]))
                    (~optional (~seq #:serialize serialize) #:defaults ([serialize #'#f]))
                    (~optional (~seq #:deserialize deserialize) #:defaults ([deserialize #'#f]))
                    (~optional (~seq #:init init) #:defaults ([init #'#f]))
                    (~optional (~seq #:elaborator elaborator) #:defaults ([elaborator #'#f]))
                    (~once default))
              ...)))
  (define-syntax-class defstate
    #:literals (define-state)
    (pattern (define-state marked-name:id
               options:defstate-options)
             #:attr name (editor-syntax-introduce (attribute marked-name))
             #:attr getter-name (format-id this-syntax "get-~a" #'name)
             #:attr setter-name (format-id this-syntax "set-~a!" #'name)
             #:attr default #'options.default
             #:attr init #'options.init
             #:attr persistence #'options.persistence
             #:attr getter #'options.getter
             #:attr setter #'options.setter
             #:attr serialize #'options.serialize
             #:attr deserialize #'options.deserialize
             #:attr elaborator #'options.elaborator)))

(define-syntax-parameter define-elaborate
  (syntax-parser
    [de:defelaborate
     (raise-syntax-error 'define-elaborate "Use outside of define-editor is an error" this-syntax)]))

(define-syntax-parameter define-state
  (syntax-parser
    [x:defstate
     (raise-syntax-error 'define-state "Use outside of define-editor is an error" this-syntax)]))

;; We don't want to get editor classes when
;; deserializing new editors.
(define deserialize-editor-classes?
  (make-parameter #t))

;; Each editor definition has three parts:
;; 1. A phase 1 elaboration
;; 2. A submodule with interaction code
;; 3. A deserializer submodule
(define-syntax (~define-editor stx)
  (syntax-parse stx
    [(_ orig-stx name:id supclass (interfaces ...)
        (~alt (~optional (~seq #:base? b?) #:defaults ([b? #'#f]))
              (~optional (~seq #:mixin mixin)))
        ...
        (~and
         (~seq (~alt plain-state:defstate
                     (~optional elaborator:defelaborate
                                #:defaults ([elaborator.data #'this-data]
                                            [elaborator.this-editor #'this-editor]
                                            [(elaborator.body 1) (list #'#'this-editor)]))
                     internal-body) ...)
         (~seq body ...)))
     #:with marked-name (editor/user-syntax-introduce #'name)
     #:with marked-supclass (editor/user-syntax-introduce #'supclass)
     #:with elaborator-name (format-id #'orig-stx "~a:elaborate" #'name)
     #:with name-deserialize (format-id #'orig-stx "~a:deserialize" #'name)
     #:with (marked-interfaces ...) (editor/user-syntax-introduce #'(interfaces ...))
     #:with (marked-body ...) (editor/user-syntax-introduce #'(body ...) 'add)
     #:with (state:defstate ...) (editor/user-syntax-introduce #'(plain-state ...))
     #:with serialize-method (gensym 'serialize)
     #:with deserialize-method (gensym 'deserialize)
     #:with deserialize-binding-method (gensym 'deserialize-binding)
     #:with copy-method (gensym 'copy)
     #:with elaborator-method (gensym 'elaborator)
     #:with modpath-method (gensym 'get-modpath)
     #:with (state-methods ...) (for/list ([i (in-list (attribute state.getter-name))])
                                  (gensym (syntax->datum i)))
     (unless (or (eq? 'module-begin (syntax-local-context)) (eq? 'module (syntax-local-context)))
       (raise-syntax-error #f "Must be defined at the module level" #'orig-stx))
     (define m? (and (attribute mixin) #t))
     (define base? (syntax-e (attribute b?)))
     (define/syntax-parse public/override
       (if base? #'public #'override))
     #`(begin
         #,@(if m?
                (list)
                (list #'(provide elaborator-name)
                      #'(begin-for-syntax
                          (let ()
                            (define b (continuation-mark-set-first #f editor-list-key))
                            (when (and b (box? b))
                              (set-box! b (cons #'name (unbox b))))))))
         (deserializer-submod
          (provide name-deserialize)
          (#%require racket/class
                     #,(package/quote-module-path))
          (define-member-name serialize-method serial-key)
          (define-member-name deserialize-method deserial-key)
          (define-member-name deserialize-binding-method deserial-binding-key)
          (define-member-name copy-method copy-key)
          (define-member-name elaborator-method elaborator-key)
          (define-member-name modpath-method modpath-key)
          (define (get-name)
            (dynamic-require (quote-module-path ".." editor) 'name))
          (define name-deserialize
            (make-deserialize-info
             (λ (version args)
               (cond [(editor-deserialize-for-elaborator)
                      args]
                     [else
                      (define this (new (get-name)))
                      (send this deserialize-method args)
                      (send this on-state-changed)
                      this]))
             (λ ()
               (cond [(editor-deserialize-for-elaborator)
                      (values 48
                              (λ _ 56))]
                     [else
                      (define pattern (new (get-name)))
                      (values pattern
                              (λ (other)
                                (send pattern copy-method other)
                                (send pattern on-state-changed)))])))))
         (editor-submod
          (provide name)
          (#%require #,(package/quote-module-path))
          (define this-modpath
            (variable-reference->module-path-index (#%variable-reference)))
          (define-runtime-module-path-index this-filepath ".")
          (define-member-name serialize-method serial-key)
          (define-member-name deserialize-method deserial-key)
          (define-member-name deserialize-binding-method deserial-binding-key)
          (define-member-name copy-method copy-key)
          (define-member-name elaborator-method elaborator-key)
          (define-member-name modpath-method modpath-key)
          (splicing-syntax-parameterize
              ([define-state
                 (syntax-parser
                   [st:defstate
                    #`(begin
                        #,(syntax/loc #'st
                            (define-init st.marked-name st.default st.init))
                        #,(syntax/loc #'st
                            (define-getter st.marked-name st.getter-name st.getter))
                        #,(syntax/loc #'st
                            (define-setter st.marked-name st.setter-name st.setter)))])]
               [define-elaborate
                 (syntax-parser
                   [de:defelaborate
                    #'(begin)])])
            (define deserialize-binding
              (make-parameter (cons 'name-deserialize
                                    (module-path-index-join '(submod ".." deserializer) this-modpath))))
            (define-syntax marked-name (make-rename-transformer #'name))
            (define #,(if m? #'(name mixin) #'name)
              (let ()
                (define-local-member-name state-methods) ...
                (class/derived
                 orig-stx
                 (name
                  #,(if m? #'(supclass mixin) #'marked-supclass)
                  ((interface* () ([prop:serializable
                                    (make-serialize-info
                                     (λ (this)
                                       (vector
                                        EDITOR-SERIALIZE-VERSION
                                        (send this serialize-method)))
                                     deserialize-binding
                                     #t
                                     (or (current-load-relative-directory) (current-directory)))]))
                   marked-interfaces ...)
                  #f)
                 (define (elaborator-method)
                   #,(if m?
                         #'(list this-modpath #f)
                         #'(list (module-path-index-join '(submod "..") this-modpath)
                                 'elaborator-name)))
                 (public/override elaborator-method)
                 (define (serialize-method)
                   (vector #,(if base?
                                 #'#f
                                 #`(super serialize-method))
                           'name
                           (let ()
                             (define state-vars
                               `((state.marked-name
                                  ,state.marked-name
                                  ,state.persistence
                                  ,state.serialize)
                                 ...))
                             (for/hash ([var (in-list state-vars)]
                                        #:when (third var))
                               (define val (second var))
                               (define serial-proc (fourth var))
                               (values (first var) (if serial-proc
                                                       (serial-proc val)
                                                       val))))))
                 (public/override serialize-method)
                 (define (deserialize-method data)
                   (define sup (vector-ref data 0))
                   (define key (vector-ref data 1))
                   (define table (vector-ref data 2))
                   #,(if base?
                         #`(void)
                         #`(super deserialize-method (if (eq? 'name key)
                                                         sup
                                                         data)))
                   (unless (eq? key 'name)
                     (log-editor-warning "Missing data for key ~a, trying super"
                                         'name))
                   (when (eq? key 'name)
                     (void)
                     #,@(for/list ([i (in-list (attribute state.marked-name))]
                                   [p? (in-list (attribute state.persistence))]
                                   [s? (in-list (attribute state.setter))]
                                   [d? (in-list (attribute state.deserialize))])
                          (define key (syntax->datum i))
                          #`(when (hash-has-key? table '#,key)
                              (define des-proc #,d?)
                              (define maybe-other-val (hash-ref table '#,key))
                              (define other-val (if des-proc
                                                    (des-proc maybe-other-val)
                                                    maybe-other-val))
                              (let ([p* #,p?])
                                (case p*
                                  [(#t) (set! #,i other-val)]
                                  [(#f) (void)]
                                  [else (set! #,i (p* #,i other-val))]))))))
                 (public/override deserialize-method)
                 (define (copy-method other)
                   #,(if base?
                         #`(void)
                         #`(super copy-method other))
                   (set! state.marked-name (send other state-methods)) ...)
                 (public/override copy-method)
                 (define (deserialize-binding-method)
                   deserialize-binding)
                 (public/override deserialize-binding-method)
                 (define (modpath-method)
                   (list this-modpath 'name))
                 (public/override modpath-method)
                 (define/public (state-methods) state.marked-name) ...
                 marked-body ...)))))
         (begin-for-syntax
           (provide name)
           (define #,(if m? #'(name $) #'name)
             (class #,(cond [base? #'object%]
                            [m? #'(supclass $)]
                            [else #'marked-supclass])
               (super-new))))
         (define-syntax-parser elaborator-inside
           [(_ data-id:id data orig)
            (syntax-parse #'orig
              [_
               #:do [(define elaborator.data
                       (parameterize ([(dynamic-require
                                        '#,(package/quote-module-path)
                                        'editor-deserialize-for-elaborator) #t])
                         (deserialize (syntax->datum #'data))))]
               #:with elaborator.this-editor #'data-id
               elaborator.body ...])])
         (define-syntax elaborator-name
           (elaborator-transformer #'elaborator-inside)))]))

(define-for-syntax (elaborator-transformer inside)
  (syntax-parser
    [(_ data orig)
     #:with elaborator-inside inside
     #`(splicing-let ([data-id
                       (parameterize ([current-load-relative-directory (this-mod-dir)])
                         (deserialize 'data))])
         (elaborator-inside data-id data orig))]))

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

;; Mixin-editors are not at module level, and thus are not
;; implicetly provided by the ~define-editor helper macro.
(define-syntax (define-editor-mixin stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~optional (~seq #:interfaces (interfaces ...)) #:defaults ([(interfaces 1) '()]))
             (~optional (~seq #:mixins (mixins ...)) #:defaults ([(mixins 1) '()]))
             (~optional (~seq #:super $) #:defaults ([$ #'super])))
        ...
        body ...)
     #:with (marked-mixins ...) (editor/user-syntax-introduce #'(mixins ...) 'add)
     #`(begin
         (begin-for-syntax
           (let ()
             (define b (continuation-mark-set-first #f editor-mixin-list-key))
             (when (and b (box? b))
               (set-box! b (cons #'name (unbox b))))))
         (~define-editor #,stx
                         name
                         (compose #,@(reverse (attribute marked-mixins)))
                         (interfaces ...)
                         #:mixin $
                         body ...))]))
