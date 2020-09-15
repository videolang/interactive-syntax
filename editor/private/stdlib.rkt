#lang racket/base

(require "lang.rkt"
         "editor.rkt"
         racket/splicing
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

;; Because this module is part of the editor language,
;;  its base lang needs to be something more like racket/base
;;  rather than editor
(splicing-syntax-parameterize ([current-editor-lang 'racket/base]
                               [current-editor-base "editor.rkt"]
                               [current-editor-modpath-mode 'package])

  (require (for-editor "context.rkt"
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
                       racket/draw
                       racket/gui/event
                       racket/pretty
                       #;(except-in racket/gui/base
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

  (begin-for-interactive-syntax
    (define editor<$>
      (interface*
       ()
       ([prop:convertible
         (λ (this format default)
           (case format
             [(png-bytes)
              (define-values (w* h*) (send this get-extent))
              (define w (exact-ceiling (max w* 1)))
              (define h (exact-ceiling (max h* 1)))
              (define bit (make-object bitmap% w h))
              (send this draw (new bitmap-dc% [bitmap bit]) 0 0)
              (define s (open-output-bytes))
              (send bit save-file s 'png)
              (get-output-bytes s)]
             [else default]))])
       partial-extent
       (get-extent (->m (values real? real?)))
       (resize (->m real? real? any/c))
       (draw (->m (is-a?/c dc<%>) real? real? any))
       (get-count (->m integer?))
       (on-event (->m (is-a?/c event%) real? real? any))
       (set-context (->m any/c #;(or/c #f (is-a?/c editor-context<$>)) void?))
       (get-context (->m any/c #;(or/c #f (is-a?/c editor-context<$>))))))

    (define current-system-font (make-parameter #f))

    ;; DC used internally for measuring text size.
    (define text-size-dc
      (new bitmap-dc% [bitmap (make-object bitmap% 1 1)])))

  (define-base-editor* base$ object% (editor<$>)
    (super-new)
    (define context #f)
    (define/public (copy)
      (deserialize (serialize this)))
    (define/pubment (draw dc x y)
      (dynamic-wind
       (λ () (send dc translate x y))
       (λ () (inner (void) draw dc))
       (λ () (send dc translate (- x) (- y)))))
    (define/public (partial-extent x y len)
      (values 0 0))
    (define/pubment (get-extent)
      (inner (values 0 0) get-extent))
    (define/public (get-count)
      1)
    (define/pubment (on-event event x y)
      (define x* (inexact->exact (round x)))
      (define y* (inexact->exact (round y)))
      (define old-x (send event get-x))
      (define old-y (send event get-y))
      (dynamic-wind
       (λ ()
         (send event set-x (- old-x x*))
         (send event set-y (- old-y y*)))
       (λ () (inner (void) on-event event))
       (λ ()
         (send event set-x old-x)
         (send event set-y old-y))))
    (define/public (description)
      "Empty Editor")
    (define/public (set-context c)
      (log-editor-debug "Setting context to ~a" c)
      (set! context c))
    (define/public (resize w h)
      #f)
    (define/public (get-context)
      context))

  (define-interactive-syntax-mixin get-path$$
    (inherit get-context)
    (super-new)
    (define/public (get-path)
      (define ctx (get-context))
      (and ctx
           (send ctx get-path))))

  (begin-for-interactive-syntax
    (define callable<$>
      (interface* ()
                  ([prop:procedure
                    (λ (this . args)
                      (send/apply this apply args))])
                  apply)))
  
  (define-interactive-syntax-mixin signaler$$
    #:interfaces (signaler<$>)
    (inherit get-persistence)
    (init [(ir receiver) '()])
    (define-state receivers (mutable-set))
    (define-state callback #f
      #:persistence (get-persistence)
      #:init #t)
    (super-new)
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

  (begin-for-interactive-syntax
    (define signaler<$>
      (interface ()
        [signal (->m any/c void?)]
        [register-receiver (->m (recursive-contract (is-a?/c receiver<$>)) void?)]
        [unregister-receiver (->m (recursive-contract (is-a?/c receiver<$>)) void?)]))
    (define receiver<$>
      (interface ()
        (on-receive (->m (is-a?/c signaler<$>) any/c void?)))))

  (define-interactive-syntax widget$ (get-path$$ base$)
    (super-new)
    (init [(ip persistence) #f])
    (define persist ip)
    (define/public (get-persistence) persist)
    (define-state parent #f
      #:init #t
      #:persistence (get-persistence)
      #:getter #t)
    (define-state top-margin 1
      #:init #t
      #:persistence (get-persistence))
    (define-state bottom-margin 1
      #:init #t
      #:persistence (get-persistence))
    (define-state left-margin 1
      #:init #t
      #:persistence (get-persistence))
    (define-state right-margin 1
      #:init #t
      #:persistence (get-persistence))
    (define/public (get-margin)
      (values top-margin bottom-margin left-margin right-margin))
    (define/public (set-margin l t r b)
      (set! left-margin l)
      (set! right-margin r)
      (set! top-margin t)
      (set! bottom-margin b))
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
    (define-state count 1
      #:persistence (get-persistence)
      #:setter (λ (c)
                 (define con (send this get-context))
                 (set! count c)
                 (when con
                   (send con recount))))
    (define/override (get-count)
      count)
    (define/augment (draw dc)
      (define-values (width height) (get-extent))
      (define old-pen (send dc get-pen))
      (define old-brush (send dc get-brush))
      (send dc set-pen
            (new pen%
                 [style 'transparent]))
      (send dc set-brush (send this get-background))
      (send dc draw-rectangle 0 0 width height)
      (send dc set-pen old-pen)
      (send dc set-brush old-brush)
      (dynamic-wind
       (λ () (send dc translate left-margin top-margin))
       (λ () (inner (void) draw dc))
       (λ () (send dc translate (- left-margin) (- top-margin)))))
    (define/public (get-content-extent)
      (inner (values 0 0) get-extent))
    (define/augment (get-extent)
      (define-values (iw ih)
        (inner (values 0 0) get-extent))
      (values (+ iw left-margin right-margin)
              (+ ih top-margin bottom-margin)))
    (define/override (resize w h)
      ;(define c (send this get-context))
      ;(when c
      ;  (send c resize content-width content-height))
      ;(when parent
      ;  (send parent resized-child this)))
      #f)
    (define/public (register-parent other)
      (set! parent other))
    (define/public (in-bounds? event)
      (define-values (width height) (get-extent))
      (define mouse-x (send event get-x))
      (define mouse-y (send event get-y))
      (and (<= 0 mouse-x width)
           (<= 0 mouse-y height)))
    (when parent
      (send parent add-child this)))

  (begin-for-interactive-syntax
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
        (set-focus (->m boolean? any))))

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
        ;; Clear all children from the parent
        (clear (or/c (->*m () () any)
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

  (define-interactive-syntax blank$ widget$
    (super-new)
    (define-state width 0
      #:getter #t
      #:setter #t
      #:persistence #f
      #:init #t)
    (define-state height 0
      #:getter #t
      #:setter #t
      #:persistence #f
      #:init #t)
    (define/augride (get-extent)
      (values width height)))

  (define-interactive-syntax pasteboard$ widget$
    #:interfaces (parent<$>)
    (inherit in-bounds? get-persistence)
    (define-state extra-width 100
      #:persistence #f
      #:init #t)
    (define-state extra-height 100
      #:persistence #f
      #:init #t)
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
      (set! children (dict-set children child (cons x y))))
    (define/public (remove-child [child #f])
      (define elem
        (or child (let ([k (dict-iterate-first children)])
                    (and k (dict-iterate-key children k)))))
      (when elem
        (set! children (dict-remove children elem))
        (when (equal? focus elem)
          (set! focus #f))))
    (define/public (clear)
      (set! children (hash))
      (set! focus #f))
    (define/public (move-child child new-x new-y)
      (set! children
            (dict-update children child (cons new-x new-y))))
    (define/public (set-child-focus [child #f])
      (set-focus! child)
      (and focus #t))
    (define/public (get-child-position child)
      (define coords (dict-ref children child
                               (λ ()
                                 (error 'pasteboard$ "Couldn't find editor ~a" child))))
      (values (car coords) (cdr coords)))
    (define/public (next-child-focus #:wrap [wrap #f])
      (error "TODO"))
    (define/public (previous-child-focus #:wrap [wrap #f])
      (error "TODO"))
    (define/public (child-focus-changed child)
      (error "TODO"))
    (define/public (resized-child child)
      (error "TODO"))
    (define/augment (on-event event)
      (cond [(is-a? event mouse-event%)
             (when (in-bounds? event)
               (match (send event get-event-type)
                 ;; Set focus or move child
                 ['left-down
                  (define old-x (send event get-x))
                  (define old-y (send event get-y))
                  (define maybe-new-focus
                    (for/fold ([focus #f])
                              ([(child pos) (in-dict children)])
                      ;(printf "~a~a~a~a~n" child pos old-x old-y)
                      (dynamic-wind
                       (λ ()
                         (send event set-x (- old-x (car pos)))
                         (send event set-y (- old-y (cdr pos))))
                       (λ ()
                         (or (and (send child in-bounds? event)
                                  child)
                             focus))
                       (λ ()
                         (send event set-x old-x)
                         (send event set-y old-y)))))
                  (cond
                    [maybe-new-focus (set! focus maybe-new-focus)]
                    [else
                     (when focus
                       (set! children (dict-set children focus
                                                (cons (send event get-x)
                                                      (send event get-y)))))])]
                 [_ (void)]))]))
    (define/augride (get-extent)
      (for/fold ([min-width min-width]
                 [min-height min-height]
                 #:result (values (+ min-width extra-width)
                                  (+ min-height extra-height)))
                ([(child pos) (in-dict children)])
        (define x (car pos))
        (define y (cdr pos))
        (define-values (w h) (send child get-extent))
        (values (max min-width (+ x w))
                (max min-height (+ y h)))))
    (define/augride (draw dc)
      (for ([(child pos) (in-dict children)])
        (send child draw dc (car pos) (cdr pos)))))

  ;; Generic list collection, used by other editors such as vertical-block$
  ;; and horizontal-block$.
  (define-interactive-syntax-mixin list-block$$
    #:interfaces (parent<$> stretchable<$>)
    (inherit get-persistence)
    (init [(ixe x-extent)]
          [(iye y-extent)]
          [(ixd x-draw)]
          [(iyd y-draw)]
          [(ixo x-offset)]
          [(iyo y-offset)])
    (define x-extent ixe)
    (define y-extent iye)
    (define x-draw ixd)
    (define y-draw iyd)
    (define x-offset ixo)
    (define y-offset iyo)
    (define-state editor-list '()
      #:getter #t
      #:persistence (get-persistence))
    (define-state focus #f)
    (super-new)
    (define/public (add-child editor [index #f])
      (cond [index
             (define-values (head tail) (split-at editor-list index))
             (set! editor-list (append head (list editor) tail))
             (send editor register-parent this)
             (resized-child editor)]
            [else
             (set! editor-list (append editor-list (list editor)))]))
    (define/public (update-child index proc)
      (define removed-editor (list-ref editor-list index))
      (set! editor-list (list-update editor-list index proc))
      (define new-editor (list-ref editor-list index))
      (send removed-editor register-parent #f)
      (send new-editor register-parent this)
      (resized-child removed-editor)
      (resized-child new-editor))
    (define/public (remove-child [editor #f])
      (when editor
        (writeln editor-list)
        (writeln editor)
        (writeln (index-of editor-list editor)))
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
    (define/public (clear)
      (for ([child (in-list editor-list)])
        (send child register-parent #f)
        (resized-child child))
      (set! editor-list '()))
    (define/public (count)
      (length editor-list))
    (define/public (in-children)
      (in-list editor-list))
    (define/public (resized-child child)
      (match-define-values (_ w h) (get-child-extents))
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
    (define/augride (get-extent)
      (define-values (extents w h)
        (send this get-child-extents))
      (log-editor-debug "List Extent: ~a" (list extents w h))
      (values w h))
    (define/public (get-child-extents)
      (get-fixed-child-extents))
    (define/private (get-fixed-child-extents #:stretchable? [stretchable? #f])
      (for/fold ([res '()]
                 [w 0]
                 [h 0]
                 #:result (values (reverse res)
                                  w h))
                ([i (in-list editor-list)])
        (cond
          [(and stretchable? (is-a? i stretchable<$>))
           (define-values (w* h*)
             (send i get-min-extent 0 0))
           (values (cons (list w* h*) res)
                   (x-extent w w*)
                   (y-extent h h*))]
          [else
           (define-values (w* h*) (send i get-extent))
           (values (cons (list w* h*) res)
                   (x-extent w w*)
                   (y-extent h h*))])))
    (define child-locs (make-hasheq))
    (define/augment (draw dc)
      (hash-clear! child-locs)
      (define-values (extents w h) (get-child-extents))
      (for/fold ([x 0]
                 [y 0])
                ([i (in-list editor-list)]
                 [e (in-list extents)])
        (define w* (first e))
        (define h* (second e))
        (define x* (x-offset x w* w))
        (define y* (y-offset y h* h))
        (hash-set! child-locs i (cons x* y*))
        (send i draw dc x* y*)
        (values (x-draw x w*)
                (y-draw y h*)))
      (inner (void) draw dc)
      (void))
    (define/public (draw-stretched dc x y w h)
      (send this draw dc x y))
    (define/augment (on-event event)
      (cond [(and (is-a? event key-event%)
                  (eq? #\tab (send event get-key-code)))
             (next-child-focus)]
            [else (for/list ([i (in-list editor-list)])
                    (define loc (hash-ref child-locs i (cons 0 0)))
                    (send i on-event event (car loc) (cdr loc)))])))

  ;; A style is: 'left, 'right, 'center
  (define-interactive-syntax vertical-block$ (list-block$$ widget$)
    (init [alignment 'left])
    (super-new [x-extent max]
               [y-extent +]
               [x-draw (λ (acc new) acc)]
               [y-draw +]
               [x-offset (λ (x cw w)
                           (case alignment
                             [(left) x]
                             [(center) (+ x (/ (- w cw) 2))]
                             [(right) (+ x (- w cw))]
                             [else x]))]
               [y-offset (λ (y ch h)
                           y)]))

  ;; A style is: 'top, 'botton, 'center
  (define-interactive-syntax horizontal-block$ (list-block$$ widget$)
    (init [alignment 'top])
    (super-new [x-extent +]
               [y-extent max]
               [x-draw +]
               [y-draw (λ (acc new) acc)]
               [x-offset (λ (x cw w)
                           x)]
               [y-offset (λ (y ch h)
                           (case alignment
                             [(top) y]
                             [(center) (+ y (/ (- h ch) 2))]
                             [(bottom) (+ y (- h ch))]
                             [else y]))]))

  (define-interactive-syntax grid-block$ widget$
    #:interfaces (parent<$>)
    (super-new)
    (define children (make-hasheq))
    (define grid (make-hasheq))
    (define child-locs (make-hasheq)) ;; (for events)
    (define (find-by-pos x y)
      (hash-ref grid (cons x y) #f))
    (define/public (add-child child [x 0] [y 0] [width 1] [height 1])
      (hash-set! children child (list x y width height))
      (for ([x* (in-range x (+ x width))]
            [y* (in-range y (+ y height))])
        (hash-set! grid (cons x* y*) child)))
    (define/public (remove-child [child* #f])
      (define child (or child* (hash-iterate-key children (hash-iterate-first children))))
      (define loc (hash-ref children child))
      (hash-remove! children child)
      (define x (first loc))
      (define y (second loc))
      (define w (third loc))
      (define h (fourth loc))
      (for ([x* (in-range x (+ x w))]
            [y* (in-range y (+ y h))])
        (hash-remove! grid (cons x* y*))))
    (define/public (clear)
      (set! children (make-hasheq))
      (set! grid (make-hasheq)))
    (define (get-child-extents)
      (define row-extents (make-hash))
      (define col-extents (make-hash))
      (define size-w 0)
      (define size-h 0)
      (for ([(child size) children])
        (define-values (child-w child-h) (send child get-extent))
        (define x (first size))
        (define y (second size))
        (define w (third size))
        (define h (fourth size))
        (define cell-w (/ child-w w))
        (define cell-h (/ child-h h))
        (set! size-w (max size-w (+ x w)))
        (set! size-h (max size-h (+ y h)))
        (for ([x* (in-range x (+ x w))]
              [y* (in-range y (+ y h))])
          (hash-update! col-extents x* (λ (old)
                                         (max old cell-w))
                        cell-h)
          (hash-update! row-extents y* (λ (old)
                                         (max old cell-h))
                        cell-w)))
      (define-values (width col-pos)
        (for/fold ([acc 0]
                   [pos '()]
                   #:result (values acc (reverse pos)))
                  ([i (in-range size-w)])
          (values
           (+ acc (hash-ref col-extents i 0))
           (cons acc pos))))
      (define-values (height row-pos)
        (for/fold ([acc 0]
                   [pos '()]
                   #:result (values acc (reverse pos)))
                  ([i (in-range size-h)])
          (values
           (+ acc (hash-ref row-extents i 0))
           (cons acc pos))))
      (values width height row-pos col-pos))
    (define/augride (get-extent)
      (match-define-values (w h _ _) (get-child-extents))
      (values w h))
    (define/augride (draw dc)
      (match-define-values (_ _ rows cols) (get-child-extents))
      (set! child-locs (make-hasheq))
      (for ([(child size) children])
        (define x (first size))
        (define y (second size))
        (hash-set! child-locs child (cons (list-ref cols x) (list-ref rows y)))
        (send child draw dc (list-ref cols x) (list-ref rows y))))
    (define/public (resized-child . args)
      (error "TODO"))
    (define/public (set-child-focus . args)
      (error "TODO"))
    (define/public (child-focus-changed . args)
      (error "TODO"))
    (define/public (next-child-focus #:wrap [wrap #f] . args)
      (error "TODO"))
    (define/public (previous-child-focus #:wrap [wrap #f] . args)
      (error "TODO"))
    (define/augment (on-event event)
      (for ([(child loc) child-locs])
        (send child on-event event (car loc) (cdr loc)))))
  
  (define-interactive-syntax-mixin text$$
    #:mixins (signaler$$)
    (inherit get-persistence)
    (init [(internal-text text) ""])
    (define-state text-width 0)
    (define-state text-height 0)
    (define-state font #f
      #:getter (λ ()
                 (or font (current-system-font)))
      #:setter #t
      #:init (λ (i)
               (or i
                   (current-system-font)
                   (dynamic-require 'racket/gui/base 'normal-control-font)))
      #:deserialize (λ (lst)
                      (and lst
                           (apply make-object font% lst)))
      #:serialize (λ (f)
                    (and f
                         (list (send f get-size)
                               (send f get-face)
                               (send f get-family)
                               (send f get-style)
                               (send f get-weight)
                               (send f get-underlined)
                               (send f get-smoothing)
                               (send f get-size-in-pixels)
                               (send f get-hinting))))
      #:persistence (get-persistence))
    (define-state scale? #f)
    (define-state text ""
      #:setter (λ (t #:signal? [signal? #f])
                 (define text-size-str (if (non-empty-string? t) t "   "))
                 (match-define-values (w h _ _)
                   (send text-size-dc get-text-extent text-size-str (send this get-font)))
                 (set! text t)
                 (set! text-width w)
                 (set! text-height h)
                 (when signal?
                   (send this signal (new control-event% [event-type 'text-field]))))
      #:getter #t
      #:persistence (get-persistence))
    (super-new)
    (define/augment (get-extent)
      (define-values (w h) (inner (values 0 0) get-extent))
      (values (max w text-width) (max h text-height)))
    (define/augride (draw dc)
      (define-values (l t r b) (send this get-margin))
      (define old-font (send dc get-font))
      (send dc set-font (send this get-font))
      (send dc draw-text text l t)
      (send dc set-font old-font))
    (send this set-background "white" 'transparent)
    (set-text! internal-text))

  (define-interactive-syntax label$ (text$$ widget$)
    (super-new)
    (init [(internal-text text) ""])
    (send this set-text! internal-text))

  (define-interactive-syntax-mixin focus$$
    #:interfaces (focus<$>)
    (define-state focus? #f)
    (define mouse-state 'up)
    (super-new)
    (define/public (has-focus?)
      focus?)
    (define/public (set-focus f)
      (set! focus? f))
    (define/augment (on-event event)
      (cond
        [(is-a? event mouse-event%)
         (define in-button? (send this in-bounds? event))
         (match (send event get-event-type)
           ['left-down
            (set! focus? in-button?)
            (when (and focus? (send this get-parent))
              (send (send this get-parent) child-focus-changed this))]
           [_ (void)])])
      (inner (void) on-event event)))

  (define-interactive-syntax button$ (signaler$$ (focus$$ widget$))
    (inherit has-focus?
             get-persistence)
    (super-new)
    (init [(il label) #f])
    (define mouse-state 'up)
    (define-state label #f
      #:persistence (get-persistence)
      #:getter #t
      #:setter (λ (l)
                 (set! label l)))
    (define up-color "Silver")
    (define hover-color "DarkGray")
    (define down-color "DimGray")
    (define/override (set-focus f)
      (super set-focus f)
      (set! mouse-state (if f 'hover 'up)))
    (define/augment (on-event event)
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
    (define/augment (get-extent)
      (send label get-extent))
    (define/augment (draw dc)
      (define-values (w h) (get-extent))
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
      (send dc draw-rounded-rectangle 0 0 (sub1 w) (sub1 h))
      (send dc set-pen old-pen)
      (send dc set-brush old-brush)
      (if label
          (send label draw dc 0 0)
          (error 'button$ "Missing label"))
      (inner (void) draw dc))
    (cond
      [(string? il)
       (set-label! (new label$ [text il]))]
      [(eq? il #f) (void)]
      [else (set-label! il)]))

  (define-interactive-syntax toggle$ (signaler$$ (focus$$ widget$))
    (inherit has-focus?
             get-persistence)
    (super-new)
    (define-state value #f
      #:persistence (get-persistence)
      #:getter #t
      #:setter #t)
    (define-state mouse-state 'up)
    (define-state up-color "Silver")
    (define-state focus-color "LightSkyBlue")
    (define-state down-color "DeepSkyBlue")
    (define/augment (on-event event)
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
    (define/augment (draw dc)
      (define-values (cw ch) (send this get-extent))
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
      (send dc draw-rectangle 0 0 cw ch)
      ;(send label draw dc (+ mx pl) (+ my pt))
      (send dc set-pen old-pen)
      (send dc set-brush old-brush)
      (inner (void) draw dc)))

  ;; Will be lifted into racket/list
  (begin-for-interactive-syntax
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

  (define-interactive-syntax radio$ (list-block$$ widget$)
    (inherit get-persistence
             get-editor-list)
    (define-state selected #f
      #:persistence (get-persistence))
    (define-state children '()
      #:persistence (get-persistence))
    (super-new)
    (define/override (add-child child)
      (define option (new horizontal-block$))
      (define toggle (new toggle$ [parent option]))
      (send toggle add-child child)
      (set! children (append children option))
      (super add-child toggle))
    (define/override (remove-child child)
      (define index (index-where children
                                 (λ (c) (eq? child (car c)))))
      (when index
        (define option (list-ref (get-editor-list) index))
        (set! children (remove-index children index))
        (super remove-child option))))

  (define-interactive-syntax-mixin pickable$$
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

  (define-interactive-syntax field$ (focus$$ (text$$ widget$))
    #:interfaces (stretchable<$>)
    (inherit get-extent
             get-text
             set-text!
             get-persistence)
    (super-new)
    (define-state background #f
      #:init #t)
    (send this set-background (or background "white"))
    (define/public (get-max-extent x y)
      (match-define-values (w h) (get-extent))
      (values +inf.0 h))
    (define/public (get-min-extent x y)
      (match-define-values (w h) (get-extent))
      (values 0 h))
    (define-state caret 0
      #:persistence (get-persistence))
    (define/override (draw dc)
      (super draw dc)
      (when (send this has-focus?)
        (define t (get-text))
        (define car-str (substring t 0 caret))
        (match-define-values (cx cy _ _)
          (send text-size-dc get-text-extent car-str (send this get-font)))
        (match-define-values (w h) (send this get-content-extent))
        (send dc draw-line cx 0 cx h)))
    (define/public (draw-stretched dc x y w h)
      (draw dc x y))
    (define/augment (on-event event)
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

  (define-interactive-syntax window$ vertical-block$
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

  (define-interactive-syntax dialog$ window$
    (inherit set-frame! get-frame)
    (init [title "Dialog"])
    (super-new)
    (define dialog% (dynamic-require 'racket/gui/base 'dialog%))
    (unless (get-frame)
      (set-frame! (new dialog%
                       [label title])))
    (define result #f)
    (define/public (get-result)
      result)
    (define/public (set-result! new)
      (set! result new)))

  (begin-for-interactive-syntax
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

  (define-interactive-syntax labeled-option$ horizontal-block$
    (super-new)
    (init [option values])
    (define/public (get-option)
      opt)
    (define-state font #f
      #:init #t)
    (define-state label #f
      #:init #t)
    (define-state bundle #f
      #:init #t)
    (define-state bundle-finalizer #f
      #:init #t)
    (define-state bundle-label label
      #:init #t)
    (new label$ [parent this]
         [font font]
         [text label])
    (define opt (option this))
    (when bundle
      (send bundle add-option
            bundle-label
            (λ ()
              ((or bundle-finalizer values) opt))))))
