#lang racket/base

(provide (all-defined-out)
         (rename-out [editor-snip-class snip-class]))

(require "lang.rkt"
         "editor.rkt"
         "read-editor.rkt"
         (prefix-in base: racket/base)
         racket/contract/base
         racket/class
         racket/gui/base
         racket/port
         file/convertible
         racket/match
         racket/list
         racket/serialize
         racket/format
         racket/math)

(define editor-context<$>
  (interface ()
    resize
    recount
    alert
    show))

;; ===================================================================================================

(define editor-canvas%
  (class* canvas% (editor-context<$>)
    (init-field editor)
    (match-define-values (width height _ _ _ _)
      (send editor get-extent 0 0))
    (super-new [min-width (exact-ceiling width)]
               [min-height (exact-ceiling height)]
               [stretchable-width #f]
               [stretchable-height #f]
               [paint-callback (λ (c dc)
                                 (send editor draw dc 0 0)
                                 (match-define-values (width height _ _ _ _)
                                   (send editor get-extent (send editor get-x) (send editor get-y)))
                                 (send this min-width (exact-ceiling width))
                                 (send this min-height (exact-ceiling height)))])
    (send editor set-context this)
    (define/public (resize . _)
      (void))
    (define/public (recount . _)
      (void))
    (define/public (alert . _)
      (error "TODO"))
    (define/override (on-event event)
      (send editor on-event event 0 0)
      (send this refresh))
    (define/override (on-char event)
      (send editor on-event event 0 0)
      (send this refresh))))

;; ===================================================================================================

;; Editor snip and snipclass implementations

(define editor-snip%
  (class* snip% (readable-snip<%>)
    (inherit get-flags set-flags set-snipclass)
    (init-field editor)
    (super-new)
    (set-flags (cons 'handles-events (get-flags)))
    (set-snipclass editor-snip-class)
    (send (get-the-snip-class-list) add editor-snip-class)
    (define/override (get-extent dc x y [w #f] [h #f] [d #f] [s #f] [ls #f] [rs #f])
      (define-values (w* h* l* t* r* b*) (send editor get-extent x y))
      (define (wsb! x y) (when x (set-box! x y)))
      (wsb! w w*)
      (wsb! h h*)
      (wsb! ls l*)
      (wsb! s t*)
      (wsb! rs r*)
      (wsb! d b*))
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send editor draw dc x y))
    (define/override (on-event dc x y ex ey event)
      (send editor get-extent x y) ;; TODO, remove this
      (send editor on-event event x y)
      (define admin (send this get-admin))
      (when admin
        (send admin resized this #t)))
    (define/override (copy)
      (new editor-snip%
           [editor (send editor copy)]))
    (define/private (editor-binding)
      (match-define (list binding-mod binding-name)
        (editor->elaborator editor))
      (list (serialize binding-mod) binding-name))
    (define/override (get-text offset num [flattened? #f])
      ;; Disregarding flattened? ...
      (format "#editor~s~s" (editor-binding) (serialize editor)))
    (define/public (read-special src line col pos)
      `(#%editor ,(editor-binding) ,(serialize editor)))
    (define/override (write f)
      (define text (string->bytes/utf-8 (get-text 0 0)))
      (send f put text))))
 
(define editor-snip-class%
  (class snip-class%
    (inherit set-classname)
    (super-new)
    (set-classname (~s '((lib "context.rkt" "editor" "private")
                         (lib "context-text.rkt" "editor" "private"))))
    (define/override (read f)
      (define text (send f get-bytes))
      (match-define `(#%editor ,_ ,the-editor)
        (with-input-from-string (bytes->string/utf-8 text)
          (λ ()
            (parameterize ([current-readtable (make-editor-readtable)])
              (base:read)))))
      (new editor-snip% [editor (deserialize the-editor)]))))

(define editor-snip-class (new editor-snip-class%))

;; ===================================================================================================
