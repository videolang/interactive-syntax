#lang racket/base

(module+ main
  (require racket/cmdline
           syntax/location)
  (define the-file
    (command-line
     #:args (file)
     file))

  (with-handlers* ([exn:fail? (λ (e) (dynamic-require `(submod ,the-file editor) #f))])
    (dynamic-require `(submod ,the-file editor test) #f)))
