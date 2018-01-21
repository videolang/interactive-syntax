#lang racket/base

(provide (rename-out [editor-reader reader]))
(require racket/class
         wxme)

(define editor-reader%
  (class* object% (snip-reader<%>)
    (super-new)
    (define/public (read-header version stream) (void))
    (define/public (read-snip text-only? version stream)
      (define text (send stream read-bytes 'editor))
      (cond [text-only? text]
            [else text])))) ; <- fix?

(define editor-reader (new editor-reader%))
