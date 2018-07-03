#lang editor editor/lang

(require editor/base
         math/matrix
         racket/class
         data/gvector
         (for-editor racket/math
                     racket/match
                     data/gvector))

(define-editor matrix-state$ base$
  #:interfaces (receiver<$>)
  (super-new)
  (define-state width 0
    #:getter #t
    #:setter (λ (new-width)
               (set! width new-width)
               (resize-matrix)))
  (define-state height 0
    #:getter #t
    #:setter (λ (new-height)
               (set! height new-height)
               (resize-matrix)))
  (define-state values (make-gvector)
    #:getter #t)
  (define/private (resize-matrix)
    (define new-length (* width height))
    (for ([i (in-range (- new-length (gvector-count values)))])
      (gvector-add! values 0)))
  (define/public (set-cell! row col val)
    (gvector-set! values (+ (* row width) col) val))
  (define/public (on-receive sender event)
    (cond
      [(is-a? event text-change-event%)
       (set-cell! (send sender get-row)
                  (send sender get-col)
                  (string->number (send sender get-text)))])))

(define-editor cell$ field$
  (init [(ir row) 0]
        [(ic col) 0])
  (define-state row ir
    #:getter #t
    #:persistence #f)
  (define-state col ic
    #:getter #t
    #:persistence #f)
  (super-new))

(define-editor matrix-body$ vertical-block$
  (inherit count
           remove-child
           in-children
           get-parent)
  (super-new)
  (define/public (resize-cells)
    (define-values (w h)
      (for/fold ([w 0]
                 [h 0])
                ([row (in-children)])
        (for/fold ([w w]
                   [h h])
                  ([cell (send row in-children)])
          (match-define-values (w* h* _ _ _ _)
            (send cell get-extent
                  (send cell get-x)
                  (send cell get-y)))
          (values (max w w*)
                  (max h h*)))))
    (send this set-uniform-child-size?! (list w h))
    (for ([row (in-children)])
      (send row set-uniform-child-size?! (list w h))))
  (define/public (fill-cells cells width)
    (for ([row (in-children)]
          [i (in-naturals)])
      (for ([cell (send row in-children)]
            [j (in-naturals)])
        (send cell set-text!
              (number->string (gvector-ref cells (+ (* i width) j))))))
    (resize-cells))
  ;; Change the dimentions of the matrix to the new width/height.
  (define/public (change-dimensions width height)
    (define height-diff (abs (- height (count))))
    ;; First grow rows
    (cond
      [(height . < . (count))
       (for ([_ (in-range height-diff)])
         (remove-child))]
      [(height . > . (count))
       (for ([_ (in-range height-diff)])
         (new horizontal-block$ [parent this]
              [uniform-child-size? #t]))])
    ;; Then collumns in thos rows
    (for ([row (in-children)]
          [row-index (in-naturals)])
      (define existing-width (send row count))
      (define width-diff (abs (- width existing-width)))
      (cond
        [(width . < . existing-width)
         (for ([_ (in-range width-diff)])
           (send row remove-child))]
        [(width . > . existing-width)
         (for ([_ (in-range width-diff)]
               [col-index (in-naturals existing-width)])
           (new cell$ [parent row]
                [row row-index]
                [col col-index]
                [text "0"]
                [callback (send this get-parent)]))]))
    (resize-cells)))

(define-editor matrix$ (signaler$$ vertical-block$)
  #:interfaces (receiver<$>)
  (super-new)
  (define-state state (new matrix-state$)
    #:getter #t)
  (define-elaborate this
    (define state (send this get-state))
    (vector->matrix (send state get-height)
                    (send state get-width)
                    (gvector->vector (send state get-values))))
  (define/public (on-receive sender message)
    (send state on-receive sender message)
    (send the-matrix resize-cells))
  (define/override (on-state-changed)
    (super on-state-changed)
    (send w-str set-text! (number->string (send state get-width)))
    (send h-str set-text! (number->string (send state get-height)))
    (send the-matrix change-dimensions
          (send state get-width)
          (send state get-height))
    (send the-matrix fill-cells
          (send state get-values)
          (send state get-width)))
  (define w-row (new horizontal-block$ [parent this]))
  (define h-row (new horizontal-block$ [parent this]))
  (new label$ [parent w-row] [text "Width: "])
  (define/public (w-str-callback this event)
    (define w (string->number (send this get-text)))
    (when (and w (natural? w))
      (send state set-width! w)
      (send the-matrix change-dimensions
            (send state get-width)
            (send state get-height))))
  (define w-str (new field$ [parent w-row]
                     [text (number->string (send state get-width))]
                     [callback (list this 'w-str-callback)]))
  (new label$ [parent h-row] [text "Height: "])
  (define/public (h-str-callback this event)
    (define h (string->number (send this get-text)))
    (when (and h (natural? h))
      (send state set-height! h)
      (send the-matrix change-dimensions
            (send state get-width)
            (send state get-height))))
  (define h-str (new field$ [parent h-row]
                     [text (number->string (send state get-height))]
                     [callback (list this 'h-str-callback)]))
  (define the-matrix (new matrix-body$ [parent this]
                          [uniform-child-size? #t])))

(begin-for-editor
  (module+ test
    (require (prefix-in gui: racket/gui/base)
             editor/private/context)
    (define matrix (new matrix$))
    (new editor-snip% [editor matrix])))
