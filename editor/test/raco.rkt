#lang racket/base

(module+ main
  (require racket/cmdline
           syntax/location)
  (define the-file
    (command-line
     #:args (file)
     file))

  (dynamic-require `(submod ,the-file editor test) #f))
