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
         racket/math
         racket/path)

(define editor-context<$>
  (interface ()
    resize
    recount
    alert
    show
    get-path))

;; ===================================================================================================

(define editor-canvas%
  (class* canvas% (editor-context<$>)
    (init-field editor)
    (inherit min-height min-width refresh)
    (match-define-values (width height _ _ _ _)
      (send editor get-extent 0 0))
    (super-new [min-width (exact-ceiling width)]
               [min-height (exact-ceiling height)]
               [stretchable-width #f]
               [stretchable-height #f]
               [paint-callback (λ (c dc)
                                 (send editor draw dc 0 0)
                                 (match-define-values (width height _ _ _ _)
                                   (send editor get-extent 0 0)) ;(send editor get-x) (send editor get-y)))
                                 (min-width (exact-ceiling width))
                                 (min-height (exact-ceiling height)))])
    (send editor set-context this)
    (define/public (resize w h)
      (void))
    (define/public (recount . _)
      (void))
    (define/public (alert . _)
      (error "TODO alert"))
    (define/override (on-event event)
      (send editor on-event event 0 0)
      (refresh))
    (define/override (on-char event)
      (send editor on-event event 0 0)
      (refresh))
    (define/public (get-path)
      #f)))

;; ===================================================================================================

(define editor-read-as-snip? (make-parameter #f))

;; Editor snip and snipclass implementations

(define editor-snip%
  (class* snip% (editor-context<$> readable-snip<%>)
    (inherit get-flags set-flags set-snipclass get-admin)
    ;; editor contains the actual interactive editor.
    (init-field editor
                ;; Serial is only if it cannot yet be processed because
                ;;   we need to first expand _this_ file.
                [serial-sexp #f]
                ;; The modname of the editor under edit in the current
                ;;   namespace
                [mod-name #f]
                ;; The namespace for the editor under edit
                [namespace #f])
    (when editor
      (send editor set-context this))
    (super-new)
    (set-flags (list* 'handles-events
                      'uses-editor-path
                      'width-depends-on-x
                      'width-depends-on-y
                      'height-depends-on-x
                      'height-depends-on-y
                      (get-flags)))
    (set-snipclass editor-snip-class)
    (send (get-the-snip-class-list) add editor-snip-class)
    (define/public (get-path)
      (define admin (get-admin))
      (cond
        [admin
         (define ctx (send admin get-editor))
         (define b (box #f))
         (define filename (send ctx get-filename b))
         (and (not (unbox b))
              filename)]
        [else #f]))
    (define/public (get-editor)
      editor)
    (define/public (set-editor! e)
      (when editor
        (send editor set-context #f))
      (set! editor e)
      (when e
        (send e set-context this))
      (define admin (get-admin))
      (when admin
        (send admin resized this #t)))
    (define/public (set-namespace! ns)
      (set! namespace ns))
    (define/public (set-mod-name! m)
      (set! mod-name m))
    (define/private (init-editor)
      (unless editor
        (parameterize ([current-namespace (or namespace (current-namespace))])
          (set-editor! (deserialize serial-sexp)))))
    (define/override (get-extent dc x y [w #f] [h #f] [d #f] [s #f] [ls #f] [rs #f])
      (init-editor)
      (define-values (w* h* l* t* r* b*) (send editor get-extent x y))
      (define (wsb! x y) (when x (set-box! x y)))
      (wsb! w w*)
      (wsb! h h*)
      (wsb! ls l*)
      (wsb! s t*)
      (wsb! rs r*)
      (wsb! d b*))
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (init-editor)
      (send editor draw dc x y))
    (define/override (on-char dc x y ex ey event)
      (init-editor)
      (send editor get-extent x y) ;; TODO, remove this
      (send editor on-event event x y)
      (define admin (get-admin))
      (when admin
        (send admin resized this #t)))
    (define/override (on-event dc x y ex ey event)
      (init-editor)
      (send editor get-extent x y) ;; TODO, remove this
      (send editor on-event event x y)
      (define admin (get-admin))
      (when admin
        (send admin resized this #t)))
    (define/override (copy)
      (init-editor)
      (define copy
        (with-handlers ([exn:fail?
                         (λ (e)
                           (log-warning "~s" e)
                           #f)])
          (send editor copy)))
      (define sexp (serialize editor))
      (new editor-snip%
           [editor copy]
           [serial-sexp sexp]))
    (define/public (editor-binding)
      (init-editor)
      (match-define `((,edit-mod ,edit-name) (,des-mod ,des-id) (,elab-mod ,elab-name))
        (editor->elaborator editor))
      (define filename (maybe-get-filename))
      (values
       (list (list edit-mod edit-name)
             (list des-mod des-id)
             (list elab-mod elab-name))
       #f #;(equal? (car edit-mod)
               mod-name)
       des-id))
    (define/private (serialize-data data des rel same-file?)
      (if same-file?
          (serialize+rehome data des #:relative-directory rel)
          (serialize data #:relative-directory rel)))
    (define/private (serialize-editor)
      (define-values (binding same-file? des-name) (editor-binding))
      (define f (maybe-get-filename))
      (values (serialize-data binding des-name (and f (cons f (build-path "/"))) same-file?)
              (serialize-data editor des-name (and f (cons f (build-path "/"))) same-file?)))
    (define/override (get-text offset num [flattened? #f])
      (init-editor)
      (define-values (binding serial) (serialize-editor))
      ;; Disregarding flattened? ...
      (format "#editor~s~s" binding serial))
    (define/private (maybe-get-filename)
      (define maybe-admin (get-admin))
      (define maybe-filename
        (cond [maybe-admin
               (define editor (send maybe-admin get-editor))
               (define tmp (box #f))
               (define file (send editor get-filename))
               (and (not (unbox tmp)) file)]
              [else #f]))
      #;(if (path-string? maybe-filename)
          (path-only maybe-filename)
          maybe-filename)
      maybe-filename)
    (define/public (read-special src line col pos)
      (cond [(editor-read-as-snip?) this]
            [else
             (define-values (binding serial) (serialize-editor))
             (define editor-datum `(#%editor ,binding ,serial))
             (datum->syntax #f ;#'#f
                            editor-datum
                            (vector src line col pos (string-length (format "~s" editor-datum))))]))
    (define/override (write f)
      (define text (string->bytes/utf-8 (get-text 0 0)))
      (send f put text))
    (define/public (alert . _)
      (error "TODO alert"))
    (define/override (resize w h)
      (super resize w h)
      (define a (get-admin))
      (when a
        (send a resized this #t)))
    (define/public (recount . _)
      (log-editor-warning "TODO recount")
      (void))
    (define/public (show show?)
      (void))))
 
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
      (define maybe-editor
        (with-handlers ([exn:fail? (λ (e)
                                     (log-warning "~s" e)
                                     #f)])
          (deserialize the-editor)))
      (new editor-snip%
           [editor maybe-editor]
           [serial-sexp the-editor]))))

(define editor-snip-class (new editor-snip-class%))

;; ===================================================================================================
