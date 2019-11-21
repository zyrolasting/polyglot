#lang racket/base

(provide build)
(require
  racket/cmdline
  racket/file
  racket/class
  unlike-assets/logging
  unlike-assets
  "../../main.rkt"
  "../../paths.rkt"
  "../fs.rkt"
  "shared.rkt")

(define (build)
  (command-line
    #:program "build"
    #:args (path)
    (report+summary
     (λ _
       (define-values (project compiler entry)
         (init-by-user-path! path))
       (send compiler add! entry)
       (with-handlers ([exn:fail? log-exn])
         (send compiler compile!))))))
