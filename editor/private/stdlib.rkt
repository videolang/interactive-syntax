#lang racket/base

(require "lang.rkt"
         racket/splicing
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

;; Because this module is part of the editor language,
;;  its base lang needs to be something more like racket/base
;;  rather than editor
(splicing-syntax-parameterize ([current-editor-lang 'racket/base]
                               [current-editor-base "editor.rkt"])

  (require "editor.rkt"
           (for-editor "context.rkt"
                       "event.rkt"
                       racket/match
                       racket/set
                       racket/list
                       racket/class
                       racket/serialize
                       racket/contract/base
                       racket/string
                       racket/dict
                       file/convertible
                       racket/math
                       (except-in racket/gui/base
                                  editor-snip%
                                  editor-canvas%))
           racket/contract/base
           file/convertible
           racket/set
           racket/list
           racket/math
           racket/draw
           racket/class
           racket/serialize
           racket/match
           racket/string
           racket/format)

  (provide (all-defined-out)
           (for-editor (all-defined-out)
                       (all-from-out "event.rkt")))

  (begin-for-editor
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
             [else default]))]
        [prop:equal+hash
         (list (λ (this other rec)
                 (equal? (serialize this)
                         (serialize other)))
               (λ (this rec) (equal-hash-code (serialize this)))
               (λ (this req) (equal-secondary-hash-code (serialize this))))])
       partial-extent
       (get-extent (->m real? real? (values real? real? real? real? real? real?)))
       (resize (->m real? real? any/c))
       (draw (->m (is-a?/c dc<%>) real? real? void?))
       (get-count (->m integer?))
       split
       merge
       (on-state-changed (->m void?))
       (on-event (->m (is-a?/c event%) real? real? any))
       (set-context (->m any/c #;(or/c #f (is-a?/c editor-context<$>)) void?))
       (get-context (->m any/c #;(or/c #f (is-a?/c editor-context<$>))))))

    ;; DC used internally for measuring text size.
    (define text-size-dc
      (new bitmap-dc% [bitmap (make-object bitmap% 1 1)])))

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
    (define/public (get-count)
      1)
    (define/public (split count)
      (error 'split "TODO"))
    (define/public (merge . args)
      (error 'merge "TODO"))
    (define/public (on-state-changed)
      (void))
    (define/public (on-event event x y)
      (void))
    (define/public (description)
      "Empty Editor")
    (define/public (set-context c)
      (log-editor-debug "Setting context to ~a" c)
      (set! context c))
    (define/public (get-context)
      context))

  (define-editor-mixin get-path$$
    (inherit get-context)
    (super-new)
    (define/public (get-path)
      (define ctx (get-context))
      (and ctx
           (send ctx get-path))))

  (define-editor scroller$ base$
    (super-new)
    (init [(ic content)])
    (define-state content #f)
    (define-state content-height 0)
    (define-state content-width 0)
    (define-state vertical-scrollbar-width 0)
    (define-state horizontal-scrollbar-height 0)
    (define/override (get-extent x y)
      (values (+ content-width vertical-scrollbar-width)
              (+ content-height horizontal-scrollbar-height)
              0
              0
              vertical-scrollbar-width
              horizontal-scrollbar-height))
    (define-state child #f))

  (begin-for-editor
    (define callable<$>
      (interface* ()
                  ([prop:procedure
                    (λ (this . args)
                      (send/apply this apply args))])
                  apply)))
  
  (define-editor-mixin signaler$$
    #:interfaces (signaler<$>)
    (super-new)
    (init [(ir receiver) '()]
          [(cb callback) #f])
    (define-state receivers (mutable-set))
    (define-state callback cb)
    (define/public (signal event)
      (cond
        [(procedure? callback)
         (callback this event)]
        [(is-a? callback receiver<$>)
         (send callback on-receive this event)]
        [(pair? callback)
         (dynamic-send (car callback) (cadr callback) this event)]
        [(not callback) (void)]
        [else (error 'signaler "Invalid Callback ~a" callback)])
      (for ([r (in-set receivers)])
        (send r on-receive this event)))
    (define/public (register-receiver x)
      (set-add! receivers x))
    (define/public (unregister-receiver x)
      (set-remove! receivers x))
    (cond
      [(is-a? ir receiver<$>)
       (register-receiver ir)]
      [(list? ir)
       (for ([i (in-list ir)])
         (register-receiver i))]))

  (begin-for-editor
    (define signaler<$>
      (interface ()
        [signal (->m any/c void?)]
        [register-receiver (->m (recursive-contract (is-a?/c receiver<$>)) void?)]
        [unregister-receiver (->m (recursive-contract (is-a?/c receiver<$>)) void?)]))
    (define receiver<$>
      (interface ()
        (on-receive (->m (is-a?/c signaler<$>) any/c void?)))))

  (define-editor tool-tip$ base$
    (super-new))

  (define-editor float$ base$
    (super-new))

  (define-editor widget$ (get-path$$ base$)
    (super-new)
    (init [(internal-parent parent) #f]
          [(ip persist) #f])
    (define persist ip)
    (define/public (get-persistence) persist)
    (define-state x 0
      #:getter #t
      #:persistence (get-persistence))
    (define-state y 0
      #:getter #t
      #:persistence (get-persistence))
    (define-state min-width 0
      #:init #t
      #:getter #t
      #:persistence (get-persistence))
    (define-state min-height 0
      #:init #t
      #:getter #t
      #:persistence (get-persistence))
    (define/private (set-pos! x* y*)
      (set! x x*)
      (set! y y*))
    (define-state content-width 0
      #:persistence (get-persistence))
    (define-state content-height 0
      #:persistence (get-persistence))
    (define-state top-margin 1
      #:persistence (get-persistence))
    (define-state bottom-margin 1
      #:persistence (get-persistence))
    (define-state left-margin 1
      #:persistence (get-persistence))
    (define-state right-margin 1
      #:persistence (get-persistence))
    (define/public (get-margin)
      (values top-margin bottom-margin left-margin right-margin))
    (define/public (set-margin l t r b)
      (set! left-margin l)
      (set! right-margin r)
      (set! top-margin t)
      (set! bottom-margin b)
      (resize (+ content-width l r) (+ content-height t b)))
    (define-state background '("Gainsboro" solid #f #f #f)
      #:persistence (get-persistence)
      #:getter (λ ()
                 (define color
                   (match (first background)
                     [(list r g b a) (make-object color% r g b a)]
                     [str str]))
                 (new brush%
                      [color color]
                      [style (or (second background) 'solid)]
                      [stipple (third background)]
                      ;[gradient (fourth background)]
                      [transformation (fifth background)])))
    (define/public (set-background brush/color [style #f])
      (define (color->quad c)
        (cond
          [(is-a? c color%)
           (list (send c red) (send c green) (send c blue) (send c alpha))]
          [else c]))
      (set! background
            (cond
              [(is-a? brush/color brush%)
               (list (color->quad (send brush/color get-color))
                     (send brush/color get-style)
                     (send brush/color get-stipple)
                     #f ;(send brush/color get-gradient)
                     (send brush/color get-transformation))]
              [else (list (color->quad brush/color) (or style 'solid) #f #f #f)])))
    (define-state parent #f
      #:persistence (get-persistence))
    (define-state count 1
      #:persistence (get-persistence)
      #:setter (λ (c)
                 (define con (send this get-context))
                 (set! count c)
                 (when con
                   (send con recount))))
    (define/override (get-count)
      count)
    (define/override (draw dc x y)
      (set-pos! x y)
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
      (set! content-width (max min-width w))
      (set! content-height (max min-height h))
      (define c (send this get-context))
      (when c
        (send c resize content-width content-height))
      (when parent
        (send parent resized-child this)))
    (define/override (get-extent x y)
      (set-pos! x y)
      (values (+ content-width left-margin right-margin)
              (+ content-height top-margin bottom-margin)
              left-margin top-margin right-margin bottom-margin))
    (define/override (resize w h)
      (define c-w (- w left-margin right-margin))
      (define c-h (- h top-margin bottom-margin))
      (set! min-width c-w)
      (set! min-height c-h)
      (local-resize-content c-w c-h))
    (define/public (get-parent)
      parent)
    (define/public (register-parent other)
      (set! parent other))
    (define/public (in-bounds? event)
      (define mouse-x (send event get-x))
      (define mouse-y (send event get-y))
      (and (<= x mouse-x (+ x content-width))
           (<= y mouse-y (+ y content-height))))
    (when internal-parent
      (send internal-parent add-child this)))

  (begin-for-editor
    ;; An interface for widgets that can be resized, like a maximized window.
    (define stretchable<$>
      (interface ()
        ;; Get's the minimum possible extent for this widget.
        ;; The input arguments are (x, y) coordinates. They are the coordinates 
        ;; where the widget will be drawn.
        ;; A return value of (0, 0) means the window can have no size.
        (get-min-extent (->m real? real? (values real? real?)))
        ;; Like get-min-extent, but for maximum possible widget size.
        ;; A return value of (+inf.0, +inf.0) means the window has no
        ;; maximum size.
        (get-max-extent (->m real? real? (values real? real?)))
        ;; A specialized draw function to render the widget at a specific size.
        ;; The dc<%> is the context that its being drawn onto.
        ;; The remaining `real?`s are the (x, y) values to place the widget,
        ;; as well as the (w, h) of the widget drawing. (These values are
        ;; expected to be between the minimum and maximum extent for the widget)
        (draw-stretched (->m (is-a?/c dc<%>) real? real? real? real? any))))

    ;; A focusable widget is one that directly can take focus on the screen.
    ;; Such as a button/text box/etc.
    (define focus<$>
      (interface ()
        ;; Returns #t if the current widget has focus, #f otherwise.
        (has-focus? (->m boolean?))
        ;; Manually sets the focus for the widget.
        (set-focus (->m boolean? any))
        ;; A more automatic way to set focus based on certain events.
        ;; The widget is responsible for notifying the parent when it takes focus.
        (on-event (->m (is-a?/c event%) real? real? any))))

    ;; A parent can be any type of widget that contains children.
    ;; Such as a list-block$$. This interface assumes that there is some
    ;; sort of order for the `next-child-focus` and `previous-child-focus`,
    ;; but not particular semantic meaning is otherwise required
    (define parent<$>
      (interface ()
        ;; Adds a new child to the collection
        (add-child (or/c (->m (is-a?/c editor<$>) any)
                         any/c))
        ;; Remove an existing child from the collection
        (remove-child (or/c (->*m () ((is-a?/c editor<$>)) any)
                            any/c))
        ;; Call (generally from a child) when they have been resized.
        ;; This gives the parent a chance to adjust its other children.
        (resized-child (->m (is-a?/c editor<$>) any))
        ;; Sets the given child as the editor's current focus.
        ;; Such as a highlighted button or current text field.
        ;; Returns #f if no existing child could be (possibly transitively)
        ;; found.
        (set-child-focus (->*m () ((or/c (is-a?/c editor<$>) #f)) boolean?))
        ;; Like set-child-focus, but does not inform the child of its focus change.
        ;; To be called by the child itself.
        ;; As a side effect, all other children loose focus.
        (child-focus-changed (->m (or (is-a?/c editor<$>) #f) any))
        ;; Move focus to the next child (button/text field) that can have focus.
        ;; This operation is also transitive accross parents.
        ;; If #:wrap is true and when there is no next child, than the widget wraps around and
        ;; focuses on the first child.
        ;; If #:wrap is false, then no child gets focus.
        ;; Returns the child that got focus, #f otherwise.
        (next-child-focus (->*m () (#:wrap boolean?) (or/c (is-a?/c editor<$>) #f)))
        ;; Like next-child-focus, but goes to the previous focusable child instead.
        (previous-child-focus (->*m () (#:wrap boolean?) (or/c (is-a?/c editor<$>) #f))))))

  (define-editor pasteboard$ widget$
    #:interfaces (parent<$>)
    (inherit in-bounds? get-persistence)
    (define-state min-height 0
      #:persistence #f
      #:init #t)
    (define-state min-width 0
      #:persistence #f
      #:init #t)
    (define-state children (hash)
      #:persistence (get-persistence)
      #:getter #t)
    (define-state focus #f
      #:persistence #f
      #:getter #t
      #:setter (λ ([child #f])
                 (set! focus child)))
    (super-new)
    (define/public (add-child child [x 0] [y 0])
      (set! children (hash-set children child (cons x y))))
    (define/public (remove-child [child #f])
      (define elem
        (or child (let ([k (dict-iterate-first children)])
                    (and k (dict-iterate-key k)))))
      (when elem
        (set! children (dict-remove children elem))
        (when (equal? focus elem)
          (set! focus #f))))
    (define/public (move-child child new-x new-y)
      (set! children
            (dict-update children child (cons new-x new-y))))
    (define/public (set-child-focus [child #f])
      (set-focus! child)
      (and focus #t))
    (define/public (next-child-focus #:wrap [wrap #f])
      (error "TODO"))
    (define/public (previous-child-focus #:wrap [wrap #f])
      (error "TODO"))
    (define/public (child-focus-changed child)
      (error "TODO"))
    (define/public (resized-child child)
      (error "TODO"))
    (define/override (on-event event x y)
      (super on-event event x y)
      (cond [(is-a? event mouse-event%)
             (when (in-bounds? event)
               (match (send event get-event-type)
                 ;; Set focus or move child
                 ['left-down
                  (define maybe-new-focus
                    (for/fold ([focus #f])
                              ([(child pos) (in-dict children)])
                      (or (and (send child in-bounds? event)
                               child)
                          focus)))
                  (cond
                    [maybe-new-focus (set! focus maybe-new-focus)]
                    [else
                     (when focus
                       (set! children (dict-set children focus
                                                (cons (send event get-x)
                                                      (send event get-y)))))])]
                 [_ (void)]))]))
    (define/override (get-extent x y)
      (define-values (w h l t r b) (super get-extent x y))
      (for/fold ([min-width (max min-width w)]
                 [min-height (max min-height h)]
                 #:result (values min-width
                                  min-height
                                  l t r b))
                ([(child pos) (in-dict children)])
        (define x (car pos))
        (define y (cdr pos))
        (define-values (w h l t r b) (send child get-extent x y))
        (values (max min-width (+ x w))
                (max min-height (+ y h)))))
    (define/override (draw dc x y)
      (super draw dc x y)
      (for ([(child pos) (in-dict children)])
        (send child draw dc (+ x (car pos)) (+ y (cdr pos))))))
  
  ;; Generic list collection, used by other editors such as vertical-block$
  ;; and horizontal-block$.
  (define-editor-mixin list-block$$
    #:interfaces (parent<$> stretchable<$>)
    (inherit/super get-extent)
    (inherit get-persistence)
    (init [(ixe x-extent)]
          [(iye y-extent)]
          [(ixd x-draw)]
          [(iyd y-draw)]
          [(ucs uniform-child-size?) #f])
    (define x-extent ixe)
    (define y-extent iye)
    (define x-draw ixd)
    (define y-draw iyd)
    (define-state uniform-child-size? ucs
      #:persistence (get-persistence)
      #:getter #t
      #:setter #t)
    (define-state editor-list '()
      #:getter #t
      #:persistence (get-persistence))
    (define-state focus #f)
    (super-new)
    (define/public (add-child editor)
      (set! editor-list (append editor-list (list editor)))
      (send editor register-parent this)
      (resized-child editor))
    (define/public (remove-child [editor #f])
      (when (empty? editor-list)
        (error 'remove-editor "List widget already empty"))
      (define index (if editor
                        (index-of editor-list editor)
                        (sub1 (length editor-list))))
      (remove-child/index index))
    (define/public (remove-child/index index)
      (define removed-editor (list-ref editor-list index))
      (send removed-editor register-parent #f)
      (set! editor-list (remq removed-editor editor-list))
      (resized-child removed-editor))
    (define/public (count)
      (length editor-list))
    (define/public (in-children)
      (in-list editor-list))
    (define/public (resized-child child)
      (match-define-values (_ w h _ _ _ _) (get-child-extents (send this get-x) (send this get-y)))
      (send this resize w h)
      (send this set-count! (length editor-list)))
    (define/public (child-focus-changed child)
      (when (send this get-parent)
        (send (send this get-parent) child-focus-changed this))
      (for/list ([i (in-list editor-list)]
                 #:unless (eq? i child))
        (when (is-a? i parent<$>)
          (send i set-child-focus #f))
        (when (is-a? i focus<$>)
          (send i set-focus #f))))
    (define/public (set-child-focus [child #f])
      (define ret
        (for/fold ([child child]
                   #:result (not child))
                  ([i (in-list editor-list)]
                   [index (in-naturals)])
          (define maybe-child
            (and (is-a? i parent<$>)
                 (send i set-child-focus child)))
          (cond [maybe-child
                 (set! focus index)
                 #f]
                [(eq? child i)
                 (set! focus index)
                 (send child set-focus #t)
                 #f]
                [(is-a? i focus<$>)
                 (send i set-focus #f)
                 child]
                [else child])))
      ret)
    (define/public (next-child-focus #:wrap [wrap? #t])
      (define start (or focus 0))
      ;; If direct child has focus, switch it off.
      (define start-editor (and (not (empty? editor-list))
                                (list-ref editor-list start)))
      (when (and start-editor (is-a? start-editor focus<$>))
        (send start-editor set-focus #f))
      ;; Update focus
      (let loop ([i start]
                 [looped-back? #f])
        (cond
          [(and looped-back? (= (add1 start) i)) #f]
          [((length editor-list) . <= . i)
           (cond
             [(empty? editor-list) #f]
             [wrap? (loop 0 #t)]
             [else #f])]
          [else
           (define editor-i (list-ref editor-list i))
           (cond
             [(is-a? editor-i parent<$>)
              (or (send editor-i next-child-focus #:wrap #f)
                  (loop (add1 i) looped-back?))]
             [(and (is-a? editor-i focus<$>)
                   (or (not (= i start))
                       looped-back?))
              (set! focus i)
              (send editor-i set-focus #t)
              editor-i]
             [else (loop (add1 i) looped-back?)])])))
    (define/public (previous-child-focus #:wrap [wrap? #t])
      (define start (or focus (length editor-list)))
      (error "prev-child-focus, TODO"))
    (define/public (get-min-extent x y)
      (error "min-extent TODO"))
    (define/public (get-max-extent x y)
      (error "max-extent TODO"))
    (define/public (get-child-extents sx sy)
      (if uniform-child-size?
          (get-uniform-child-extents sx sy uniform-child-size?)
          (get-fixed-child-extents sx sy)))
    (define/private (get-fixed-child-extents sx sy
                                             #:stretchable? [stretchable? #f])
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
        (cond
          [(and stretchable? (is-a? i stretchable<$>))
           (define-values (w* h*)
             (send i get-max-extent x y))
           (values (cons (list w* h*) res)
                   (x-extent w w*)
                   (y-extent h h*)
                   (x-draw x w*)
                   (y-draw y h*))]
          [else
           (define-values (w* h* l t r b)
             (send i get-extent x y))
           (values (cons (list w* h* l t r b) res)
                   (x-extent w w*)
                   (y-extent h h*)
                   (x-draw x w*)
                   (y-draw y h*))])))
    (define/private (get-uniform-child-extents sx sy
                                               [maybe-child-sizes #t])
      (match-define-values (extents x y l t r b) (get-fixed-child-extents sx sy))
      (define-values (max-width max-height)
        (if (list? maybe-child-sizes)
            (values (first maybe-child-sizes) (second maybe-child-sizes))
            (for/fold ([max-width 0]
                       [max-height 0])
                      ([i (in-list extents)])
              (values (max max-width (first i))
                      (max max-height (second i))))))
      (define-values (modified-extents new-width new-height)
        (for/fold ([exts '()]
                   [mw (+ l r)]
                   [mh (+ t b)]
                   #:result (values (reverse exts) mw mh))
                  ([i (in-list extents)])
          (values (cons (list max-width max-height (third i) (fourth i) (fifth i) (sixth i))
                        exts)
                  (x-extent max-width mw)
                  (y-extent max-height mh))))
      (values modified-extents new-width new-height l t r b))
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
    (define/public (draw-stretched dc x y w h)
      (send this draw dc x y))
    (define/override (on-event event x y)
      (super on-event event x y)
      (cond [(and (is-a? event key-event%)
                  (eq? #\tab (send event get-key-code)))
             (next-child-focus)]
            [else (for/list ([i (in-list editor-list)])
                    (send i on-event event 0 0))])))

  (define-editor vertical/horizontal-block$ (list-block$$ widget$)
    (init [style 'vertical])
    (case style
      [(vertical)
       (super-new [x-extent max]
                  [y-extent +]
                  [x-draw (λ (acc new) acc)]
                  [y-draw +])]
      [(horizontal)
       (super-new [x-extent +]
                  [y-extent max]
                  [x-draw +]
                  [y-draw (λ (acc new) acc)])]
       [else (error "Not a valid style")])
    (define/override (get-extent x y)
      (super get-extent x y)
      (define-values (extents w h l t r b)
        (send this get-child-extents x y))
      (log-editor-debug "Vertical Extent: ~a" (list x y extents w h))
      (values w h l t r b))
    (define/override (draw dc x y)
      (super draw dc x y)
      (send this draw-child dc x y)))

  (define-editor vertical-block$ vertical/horizontal-block$
    (super-new [style 'vertical]))

  (define-editor horizontal-block$ vertical/horizontal-block$
    (super-new [style 'horizontal]))

  (define-editor-mixin text$$
    #:mixins (signaler$$)
    (inherit get-persistence)
    (init [(internal-font font) normal-control-font]
          [(internal-text text) ""])
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
    (define-state text ""
      #:setter (λ (t #:signal? [signal? #f])
                 (define text-size-str (if (non-empty-string? t) t "   "))
                 (match-define-values (w h _ _)
                   (send text-size-dc get-text-extent text-size-str (send this get-font)))
                 (set! text t)
                 (set! text-width w)
                 (set! text-height h)
                 (send this resize-content text-width text-height)
                 (when signal?
                   (send this signal (new control-event% [event-type 'text-field]))))
      #:getter #t
      #:persistence (get-persistence))
    (super-new)
    (define/override (draw dc x y)
      (super draw dc x y)
      (define-values (l t r b) (send this get-margin))
      (define old-font (send dc get-font))
      (send dc set-font (send this get-font))
      (send dc draw-text text (+ l x) (+ t y))
      (send dc set-font old-font))
    (send this set-background "white" 'transparent)
    (set-text! internal-text)
    (set-font internal-font))

  (define-editor label$ (text$$ widget$)
    (super-new)
    (init [(internal-text text) ""])
    (send this set-text! internal-text))

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

  (define-editor-mixin focus$$
    #:interfaces (focus<$>)
    (define-state focus? #f)
    (define mouse-state 'up)
    (super-new)
    (define/public (has-focus?)
      focus?)
    (define/public (set-focus f)
      (set! focus? f))
    (define/override (on-event event x y)
      (super on-event event x y)
      (cond
        [(is-a? event mouse-event%)
         (define in-button? (send this in-bounds? event))
         (match (send event get-event-type)
           ['left-down
            (set! focus? in-button?)
            (when (and focus? (send this get-parent))
              (send (send this get-parent) child-focus-changed this))]
           [_ (void)])])))

  (define-editor button$ (signaler$$ (focus$$ (padding$$ widget$)))
    (inherit resize-content has-focus?)
    (super-new)
    (init [(il label) #f])
    (define mouse-state 'up)
    (define-state label #f
      #:getter #t
      #:setter (λ (l)
                 (set! label l)
                 (match-define-values (w h _ _ _ _)
                   (send l get-extent (send l get-x) (send l get-y)))
                 (resize-content w h)))
    (define-state up-color "Silver")
    (define-state hover-color "DarkGray")
    (define-state down-color "DimGray")
    (define/override (set-focus f)
      (super set-focus f)
      (set! mouse-state (if f 'hover 'up)))
    (define/override (on-event event x y)
      (super on-event event x y)
      (cond
        [(is-a? event mouse-event%)
         (define in-button?
           (send this in-bounds? event))
         (match (send event get-event-type)
           ['left-down
            (when (and in-button? (eq? mouse-state 'hover))
              (set! mouse-state 'down))]
           ['left-up
            (when (and in-button? (eq? mouse-state 'down))
              (if in-button?
                  (set! mouse-state 'hover)
                  (set! mouse-state 'up))
              (define control-event (new control-event% [event-type 'button]))
              (send this signal control-event))]
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
    (when il
      (set-label! il)))

  (define-editor toggle$ (signaler$$ (focus$$ (padding$$ widget$)))
    (inherit resize-content has-focus?)
    (super-new)
    (define-state value #f
      #:getter #t
      #:setter #t)
    (define-state mouse-state 'up)
    (define-state up-color "Silver")
    (define-state focus-color "LightSkyBlue")
    (define-state down-color "DeepSkyBlue")
    (resize-content 20 20)
    (define/override (on-event event x y)
      (super on-event event x y)
      (cond
        [(is-a? event mouse-event%)
         (define in-button?
           (send this in-bounds? event))
         (match (send event get-event-type)
           ['left-down
            (when in-button?
              (set! mouse-state 'down))]
           ['left-up
            (when (and in-button? (eq? mouse-state 'down))
              (set! value (not value))
              (define control-event (new control-event% [event-type 'check-box]))
              (send this signal control-event))
            (set! mouse-state 'up)]
           [_ (void)])]))
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
                                 (cond
                                   ;[(has-focus?) focus-color]
                                   [value down-color]
                                   [else up-color]))]))
      (send dc draw-rectangle mx my (+ cw pl pr) (+ ch pt pb))
      ;(send label draw dc (+ mx pl) (+ my pt))
      (send dc set-pen old-pen)
      (send dc set-brush old-brush)))

  ;; Will be lifted into racket/list
  (begin-for-editor
    (define (remove-index ls index)
      (unless (list? ls)
        (raise-argument-error 'remove-index "list?" 0 ls index))
      (unless (exact-nonnegative-integer? index)
        (raise-argument-error 'remove-index "exact-nonnegative-integer?" 1 ls index))
      (let loop ([count 0]
                 [lst ls])
        (cond [(null? lst) '()]
              [(= count index)
               (loop (add1 count)
                     (cdr lst))]
              [else
               (cons (car lst)
                     (loop (add1 count)
                           (cdr lst)))]))))

  (define-editor radio$ vertical/horizontal-block$
    (inherit get-persistence
             get-editor-list)
    (define selected #f)
    (define-state children '()
      #:persistence (get-persistence))
    (super-new)
    (define/override (add-child child)
      (define option (new horizontal-block$))
      (define toggle (new toggle$ [parent option]))
      (set! children (append (cons children toggle) child))
      (send toggle add-child child)
      (super add-child toggle))
    (define/override (remove-child child)
      (define index (index-where children
                                 (λ (c) (eq? child (car c)))))
      (when index
        (define option (list-ref (get-editor-list) index))
        (set! children (remove-index children index))
        (super remove-child option))))

  (define-editor-mixin pickable$$
    (super-new)
    (init-field [(in normal) (void)]
                [(ih hover) (void)]
                [(ip picked) (void)])
    (define-state normal-style in)
    (define-state hover-style ih)
    (define-state picked-style ip)
    ; states can be one of: normal, hover, or picked
    (define-state state 'normal)
    (define (get-state state)
      state)
    (define (set-state! s)
      (set! state s)))

  (define-editor field$ (focus$$ (text$$ (padding$$ widget$)))
    #:interfaces (stretchable<$>)
    (inherit get-extent
             get-text
             set-text!
             get-persistence)
    (super-new)
    (send this set-background "white")
    (define/public (get-max-extent x y)
      (match-define-values (w h _ _ _ _)
        (get-extent x y))
      (values +inf.0 h))
    (define/public (get-min-extent x y)
      (match-define-values (w h _ _ _ _)
        (get-extent x y))
      (values 0 h))
    (define-state caret 0
      #:persistence (get-persistence))
    (define/override (draw dc x y)
      (super draw dc x y)
      (when (send this has-focus?)
        (define t (get-text))
        (define car-str (substring t 0 caret))
        (match-define-values (cx cy _ _)
          (send text-size-dc get-text-extent car-str (send this get-font)))
        (match-define-values (w h) (send this get-content-extent))
        (define-values (pl pt pr pb) (send this get-padding))
        (define-values (ml mt mr mb) (send this get-margin))
        (send dc draw-line (+ x pr cx) (+ y pt) (+ x pr cx) (+ y h))))
    (define/public (draw-stretched dc x y w h)
      (draw dc x y))
    (define/override (on-event event x y)
      (super on-event event x y)
      (define text (get-text))
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
              (define old-caret caret)
              (set! caret (max 0 (sub1 caret)))
              (set-text! (format "~a~a"
                                 (substring text 0 (max 0 (sub1 old-caret)))
                                 (substring text old-caret))
                         #:signal? #t)
              ]
             [#\rubout
              (error "Delete key: TODO")
              #;(set! text
                      (format "~a~a"
                              (substring text 0 caret)
                              (substring text (min (length text) (add1 caret)))))
              #;(set! caret (min (sub1 (length text)) caret))]
             [(? char?)
              (send this set-text! (format "~a~a~a"
                                          (substring text 0 caret)
                                          char
                                          (substring text caret))
                    #:signal? #t)
              (set! caret (add1 caret))]
             [_ (void)]))]))
    (let ([t (get-text)])
      (when t
        (set! caret (string-length t)))))

  (define-editor window$ vertical-block$
    (inherit get-context)
    (super-new)
    (define-state frame #f
      #:init #t
      #:setter #t
      #:getter #t)
    (define/public (show [show? #t])
      (when (get-context)
        (send (get-context) show show?))
      (when frame
        (cond
          [show?
           (new editor-canvas% [parent frame]
                [editor this])
           (send frame show #t)]
          [else
           (send frame show #f)]))))

  (define-editor dialog$ window$
    (inherit set-frame! get-frame)
    (init [title "Dialog"])
    (super-new)
    (unless (get-frame)
      (set-frame! (new dialog%
                       [label title])))
    (define result #f)
    (define/public (get-result)
      result)
    (define/public (set-result! new)
      (set! result new)))

  (begin-for-editor
    (define option-bundle$
      (class object%
        (super-new)
        (define finalized? #f)
        (define options (hash))
        (define/public (add-option label setter)
          (when finalized?
            (error 'option-bundle$ "Options already finalized"))
          (set! options (dict-set options label setter)))
        (define/public (finalize-options)
          (when finalized?
            (error 'option-bundle$ "Options already finalized"))
          (set! finalized? #t)
          (set! options
                (for/hash ([(option setter) (in-dict options)])
                  (values option (setter)))))
        (define/public (get-options)
          (unless finalized?
            (log-editor-warning 'option-bundle "Finalizing Options")
            (finalize-options))
          options))))

  (define-editor labeled-option$ horizontal-block$
    (super-new)
    (init [option values])
    (define/public (get-option)
      opt)
    (define-state label #f
      #:init #t)
    (define-state bundle #f
      #:init #t)
    (define-state bundle-finalizer #f
      #:init #t)
    (define-state bundle-label label
      #:init #t)
    (new label$ [parent this]
         [text label])
    (define opt (option this))
    (when bundle
      (send bundle add-option
            bundle-label
            (λ ()
              ((or bundle-finalizer values) opt))))))
