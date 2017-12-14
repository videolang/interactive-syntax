#lang racket/base

(provide (all-defined-out))
(require file/convertible
         racket/set
         racket/list
         racket/math
         racket/draw
         racket/class
         racket/gui/base
         racket/serialize
         racket/stxparam
         racket/splicing
         racket/match
         racket/string
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

(define editor-context<$>
  (interface ()
    resized
    recounted))

(define editor<$>
  (interface*
   ()
   ([prop:convertible
     (λ (this format default)
       (case format
         [(png-bytes)
          (define-values (w* h* l t r b) (send this get-extent 0 0))
          (define w (exact-ceiling (max w* 1)))
          (define h (exact-ceiling (max h* 1)))
          (define bit (make-object bitmap% w h))
          (send this draw (new bitmap-dc% [bitmap bit]) 0 0)
          (define s (open-output-bytes))
          (send bit save-file s 'png)
          (get-output-bytes s)]
         [else default]))])
   invalidate-extent-cache
   partial-extent
   get-extent
   resize
   draw
   get-count
   split
   merge
   on-event
   on-goodbye-event
   set-context
   get-context
   register-context))

(define-logger editor)

;; ===================================================================================================

(define editor-canvas%
  (class canvas%
    (init-field editor)
    (match-define-values (width height _ _ _ _) (send editor get-extent 0 0))
    (super-new [min-width (exact-ceiling width)]
               [min-height (exact-ceiling height)]
               [paint-callback (λ (c dc)
                                 (send editor draw dc 0 0)
                                 (match-define-values (width height _ _ _ _)
                                   (send editor get-extent 0 0))
                                 (send this min-width (exact-ceiling width))
                                 (send this min-height (exact-ceiling height)))])
    (define/override (on-event event)
      (send editor on-event event)
      (send this refresh))
    (define/override (on-char event)
      (send editor on-event event)
      (send this refresh))))

(define editor-snip%
  (class snip%
    (inherit get-flags set-flags)
    (init-field editor)
    (super-new)
    (set-flags (cons 'handles-events (get-flags)))
    (define (get-size)
      (send editor get-min-extent))
    (define/override (get-extent dc x y [w #f] [h #f] [d #f] [s #f] [ls #f] [rs #f])
      (define-values (w* h*) (get-size))
      (when w (set-box! w w*))
      (when h (set-box! h h*)))
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define-values (w h) (get-size))
      (send editor draw dc x y w h))
    (define/override (on-event dc x y ex ey event)
      (send editor on-mouse-event event))
    (define/override (copy)
      (new editor-snip%
           [editor (send editor copy)]))))

;; ===================================================================================================

(define text-size-dc
  (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))

(define-base-editor* base$ object% (editor<$>)
  (super-new)
  (define context #f)
  (define/public (copy)
    (deserialize (serialize this)))
  (define/public (draw dc x y)
    (void))
  (define/public (partial-extent x y len)
    (values 0 0))
  (define/public (get-extent x y)
    (values 0 0 0 0 0 0))
  (define/public (resize w h)
    #f)
  (define/public (invalidate-extent-cache)
    (void))
  (define/public (get-count)
    1)
  (define/public (split count)
    (error 'split "TODO"))
  (define/public (merge . args)
    (error 'merge "TODO"))
  (define/public (on-event event)
    (void))
  (define/public (on-goodbye-event event)
    (void))
  (define/public (set-context c)
    (set! context c))
  (define/public (get-context)
    context)
  (define/public (register-context ctx)
    (set! context ctx)))

(define-editor-mixin receiver$$
  (super-new)
  (define/public (on-receive event)
    (void))
  (define/public (signal event)
    (on-receive event)))

(define-editor-mixin signaler$$
  (super-new)
  (define-public-state receivers (mutable-set))
  (define/public (signal event)
    (for ([r (in-set receivers)])
      (send r signal event)))
  (define/public (register-receiver x)
    (set-add! receivers x))
  (define/public (unregister-receiver x)
    (set-remove! receivers x)))

(define-editor widget$ base$
  (super-new)
  (init [(internal-parent parent) #f])
  (define-state content-width 0)
  (define-state content-height 0)
  (define-state top-margin 1)
  (define-state bottom-margin 1)
  (define-state left-margin 1)
  (define-state right-margin 1)
  (define/public (get-margin)
    (values top-margin bottom-margin left-margin right-margin))
  (define/public (set-margin l t r b)
    (set! left-margin l)
    (set! right-margin r)
    (set! top-margin t)
    (set! bottom-margin b)
    (resize (+ content-width l r) (+ content-height t b)))
  (define-state background '("Gainsboro" solid #f #f #f))
  (define/public (get-background)
    (define color
      (match (first background)
        [(list r g b a) (make-object color% r g b a)]
        [str str]))
    (new brush%
         [color color]
         [style (or (second background) 'solid)]
         [stipple (third background)]
         ;[gradient (fourth background)]
         [transformation (fifth background)]))
  (define/public (set-background brush/color [style #f])
    (define (color->quad c)
      (cond
        [(is-a? c color%)
         (list (send c red) (send c green) (send c blue) (send c alpha))]
        [else c]))
    (cond
      [(is-a? brush/color brush%)
       (list (color->quad (send brush/color get-color))
             (send brush/color get-style)
             (send brush/color get-stipple)
             #f ;(send brush/color get-gradient)
             (send brush/color get-transformation))]
      [else (list (color->quad brush/color) (or style 'solid) #f #f #f)]))
  (define-state parent #f)
  (define-state count 1)
  (define/override (get-count)
    count)
  (define/public (set-count c)
    (define c (send this get-context))
    (set! count c)
    (when c
      (send c recounted)))
  (define/override (draw dc x y)
    (define old-pen (send dc get-pen))
    (define old-brush (send dc get-brush))
    (send dc set-pen
          (new pen%
               [style 'transparent]))
    (send dc set-brush (send this get-background))
    (send dc draw-rectangle x y
          (+ content-width left-margin right-margin)
          (+ content-height top-margin bottom-margin))
    (send dc set-pen old-pen)
    (send dc set-brush old-brush))
  (define/public (get-content-extent)
    (values content-width content-height))
  (define/public (resize-content w h)
    (local-resize-content w h))
  (define (local-resize-content w h)
    (set! content-width w)
    (set! content-height h)
    (define c (send this get-context))
    (when c
      (send c resized))
    (when parent
      (send parent resized-child this)))
  (define/override (get-extent x y)
    (values (+ content-width left-margin right-margin)
            (+ content-height top-margin bottom-margin)
            left-margin top-margin right-margin bottom-margin))
  (define/override (resize w h)
    (define c-w (- w left-margin right-margin))
    (define c-h (- h top-margin bottom-margin))
    (local-resize-content c-w c-h))
  (define/public (get-parent)
    parent)
  (define/public (register-parent other)
    (set! parent other))
  (define/public (add-child child)
    (error 'add-child "editor does not have children"))
  (define/public (remove-child child)
    (error 'remove-child "editor does not have children"))
  (define/public (resized-child child)
    (error 'resized-child "editor does not have children"))
  (when internal-parent
    (register-parent internal-parent)
    (send parent add-child this)))

(define-editor-mixin list-block$$
  (inherit/super get-extent)
  (init [(ixe x-extent)]
        [(iye y-extent)]
        [(ixd x-draw)]
        [(iyd y-draw)])
  (super-new)
  (define x-extent ixe)
  (define y-extent iye)
  (define x-draw ixd)
  (define y-draw iyd)
  (define-public-state editor-list '())
  (define/override (add-child editor)
    (set! editor-list (append editor-list (list editor)))
    (send editor register-parent this)
    (resized-child editor))
  (define/override (remove-child editor)
    (when (empty? editor-list)
      (error 'remove-editor "List widget already emtpy"))
    (send editor register-parent #f)
    (set! editor-list (take editor-list (sub1 (length editor-list))))
    (resized-child editor))
  (define/override (resized-child child)
    (match-define-values (_ w h _ _ _ _) (get-child-extents 0 0))
    (send this resize w h)
    (send this set-count (length editor-list)))
  (define/public (get-child-extents sx sy)
    (define-values (sw sh l t r b) (super get-extent sx sy))
    (for/fold ([res '()]
               [w 0]
               [h 0]
               [x (+ l sx)]
               [y (+ t sy)]
               #:result (values (reverse res)
                                (max sw (+ w l r))
                                (max sh (+ h t b))
                                l t r b))
              ([i (in-list editor-list)])
      (define-values (w* h* l t r b)
        (send i get-extent x y))
      (values (cons (list w* h* l t r b) res)
              (x-extent w w*)
              (y-extent h h*)
              (x-draw x w*)
              (y-draw y h*))))
  (define/public (draw-child dc x y)
    (match-define-values (extents _ _ l t r b) (get-child-extents x y))
    (for/fold ([x (+ l x)]
               [y (+ t y)])
              ([i (in-list editor-list)]
               [e (in-list extents)])
      (define w (first e))
      (define h (second e))
      (send i draw dc x y)
      (values (x-draw x w)
              (y-draw y h)))
    (void))
  (define/override (on-event event)
    (super on-event event)
    (for/list ([i (in-list editor-list)])
      (send i on-event event)))
  (define/override (on-goodbye-event event)
    (super on-goodbye-event event)
    (for/list ([i (in-list editor-list)])
      (send i on-goodbye-event event))))

(define-editor vertical-block$ (list-block$$ widget$)
  (super-new [x-extent max]
             [y-extent +]
             [x-draw (λ (acc new) acc)]
             [y-draw +])
  (define/override (get-extent x y)
    (define-values (extents w h l t r b)
      (send this get-child-extents x y))
    (log-editor-debug "Vertical Extent: ~a" (list x y extents w h))
    (values w h l t r b))
  (define/override (draw dc x y)
    (super draw dc x y)
    (send this draw-child dc x y)))

(define-editor horizontal-block$ (list-block$$ widget$)
  (super-new [x-extent +]
             [y-extent max]
             [x-draw +]
             [y-draw (λ (acc new) acc)])
  (define/override (get-extent x y)
    (define-values (extents w h l t r b)
      (send this get-child-extents x y))
    (log-editor-debug "Horizontal Extent: ~a" (list x y extents w h))
    (values w h l t r b))
  (define/override (draw dc x y)
    (super draw dc x y)
    (send this draw-child dc x y)))

(define-editor-mixin text$$
  (super-new)
  (init [(internal-font font) normal-control-font])
  (define-state text "")
  (define-state text-width 0)
  (define-state text-height 0)
  (define-state font '())
  (define/public (get-font)
    (apply make-object font% font))
  (define/public (set-font f)
    (set! font (list (send f get-size)
                     (send f get-face)
                     (send f get-family)
                     (send f get-style)
                     (send f get-weight)
                     (send f get-underlined)
                     (send f get-smoothing)
                     (send f get-size-in-pixels)
                     (send f get-hinting))))
  (define-state scale? #f)
  (send this set-background "white" 'transparent)
  (define/public (set-text t)
    (match-define-values (w h _ _) (send text-size-dc get-text-extent t (send this get-font)))
    (set! text t)
    (set! text-width w)
    (set! text-height h)
    (send this resize-content text-width text-height))
  (define/public (get-text t)
    text)
  (define/override (draw dc x y)
    (super draw dc x y)
    (define-values (l t r b) (send this get-margin))
    (define old-font (send dc get-font))
    (send dc set-font (send this get-font))
    (send dc draw-text text (+ l x) (+ t y))
    (send dc set-font old-font))
  (set-font internal-font))

(define-editor label$ (text$$ widget$)
  (super-new)
  (init [(internal-text text) ""])
  (send this set-text internal-text))

(define-editor-mixin padding$$
  (super-new)
  (define-state left-padding 1)
  (define-state top-padding 1)
  (define-state right-padding 1)
  (define-state bottom-padding 1)
  (define-state content-width 0)
  (define-state content-height 0)
  (define/public (get-padding)
    (values left-padding top-padding right-padding bottom-padding))
  (define/override (get-content-extent)
    (values content-width content-height))
  (define/override (resize-content w h)
    (and (super resize-content
                (+ w left-padding right-padding)
                (+ h top-padding bottom-padding))
         (set! content-width w)
         (set! content-height h)))
  (define/public (set-padding l t r b)
    (and (super resize-content (+ content-width l r) (+ content-height t b))
         (set! left-padding l)
         (set! top-padding t)
         (set! right-padding r)
         (set! bottom-padding b)
         (super resize-content (+ content-width l r) (+ content-height t b)))))

(define-editor button$ (signaler$$ (padding$$ widget$))
  (super-new)
  (init [(internal-label label) #f])
  (define mouse-state 'up)
  (define-state label #f)
  (define-state up-color "Silver")
  (define-state hover-color "DarkGray")
  (define-state down-color "DimGray")
  (define/override (on-event event)
    (super on-event event)
    (cond
      [(is-a? event mouse-event%)
       (define-values (w h l t r b)
         (send this get-extent 0 0))
       (define mouse-x (send event get-x))
       (define mouse-y (send event get-y))
       (define in-button?
         (and (<= l mouse-x r)
              (<= t mouse-y b)))
       (match (send event get-event-type)
         ['left-down
          (when (and in-button? (eq? mouse-state 'hover))
            (set! mouse-state 'down))]
         ['left-up
          (when (and in-button? (eq? mouse-state 'down))
            (if in-button?
                (set! mouse-state 'hover)
                (set! mouse-state 'up))
            (send this signal this))]
         ['motion
          (match mouse-state
            [(or 'up 'hover)
             (if in-button?
                 (set! mouse-state 'hover)
                 (set! mouse-state 'up))]
            ['down
             (unless in-button?
               (set! mouse-state 'up))])]
         [_ (void)])]))
  (define/public (set-label l)
    (set! label l)
    (match-define-values (w h _ _ _ _)
      (send l get-extent 0 0))
    (send this resize-content w h))
  (define/override (draw dc x y)
    (super draw dc x y)
    (define-values (pl pt pr pb) (send this get-padding))
    (define-values (ml mt mr mb) (send this get-margin))
    (define-values (cw ch) (send this get-content-extent))
    (define mx (+ x ml))
    (define my (+ y mt))
    (define old-pen (send dc get-pen))
    (define old-brush (send dc get-brush))
    (send dc set-pen
          (new pen% [width 1]))
    (send dc set-brush
          (new brush% [color (make-object color%
                               (match mouse-state
                                 ['up up-color]
                                 ['hover hover-color]
                                 ['down down-color]))]))
    (send dc draw-rectangle mx my (+ cw pl pr) (+ ch pt pb))
    (send label draw dc (+ mx pl) (+ my pt))
    (send dc set-pen old-pen)
    (send dc set-brush old-brush))
  (set-label internal-label))

(define-editor toggle$ widget$
  (super-new))

(define-editor radio$ (list-block$$ widget$)
  (super-new))

(define-editor-mixin focus$$
  (super-new)
  (define-state focus? #f)
  (define mouse-state 'up)
  (define/public (has-focus?)
    focus?)
  (define/override (on-event event)
    (super on-event event)
    (cond
      [(is-a? event mouse-event%)
       (define-values (w h l t r b)
         (send this get-extent 0 0))
       (define mouse-x (send event get-x))
       (define mouse-y (send event get-y))
       (define in-button?
         (and (<= l mouse-x r)
              (<= t mouse-y b)))
       (match (send event get-event-type)
         ['left-down
          (if (and in-button? (eq? mouse-state 'hover))
              (set! focus? #t)
              (set! focus? #f))]
         ['left-up
          (when (and in-button? (eq? mouse-state 'down))
            (if in-button?
                (set! mouse-state 'hover)
                (set! mouse-state 'up))
            (send this signal this))]
         ['motion
          (match mouse-state
            [(or 'up 'hover)
             (if in-button?
                 (set! mouse-state 'hover)
                 (set! mouse-state 'up))]
            ['down
             (unless in-button?
               (set! mouse-state 'up))])]
         [_ (void)])])))

(define-editor field$ (focus$$ (text$$ widget$))
  (super-new)
  (send this set-background "white")
  (define-state caret 0)
  (define/override (draw dc x y)
    (super draw dc x y)
    (when (send this has-focus?)
      (define text (send this get-text))
      (define pre-str (substring text 0 caret))
      (define the-font (send this get-font))
      (define caret-text (substring text 0 caret))
      (define-values (w h _ _*) (send text-size-dc get-text-extent caret-text the-font))
      (define old-pen (send dc get-pen))
      (send dc set-pen "black" 1 'solid)
      (send dc draw-line (+ x w) y (+ x w) (+ y h))
      (send dc set-pen old-pen)))
  (define/override (on-event event)
    (super on-event event)
    (define text (send this get-text))
    (cond
      [(is-a? event key-event%)
       (when (send this has-focus?)
         (define char (send event get-key-code))
         (match char
           ['left
            (set! caret (max 0 (sub1 caret)))]
           ['right
            (set! caret (min (string-length text) (add1 caret)))]
           [(or #\newline #\return)
            (void)]
           [#\backspace
            (send this set-text (format "~a~a"
                                        (substring text 0 (max 0 (sub1 caret)))
                                        (substring text caret)))
            (set! caret (max 0 (sub1 caret)))]
           [#\rubout
            (error "TODO")
            #;(set! text
                    (format "~a~a"
                            (substring text 0 caret)
                            (substring text (min (length text) (add1 caret)))))
            #;(set! caret (min (sub1 (length text)) caret))]
           [(? char?)
            (send this set-text (format "~a~a~a"
                                        (substring text 0 caret)
                                        char
                                        (substring text caret)))
            (set! caret (add1 caret))]
           [_ (void)]))])))
