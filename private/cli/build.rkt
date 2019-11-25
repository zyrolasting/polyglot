#lang racket/base

(provide build)
(require racket/cmdline
         "../../main.rkt"
         "../fs.rkt"
         "shared.rkt")

(define (build)
  (command-line
    #:program "build"
    #:args (path)
    (report+summary
     (λ _
       (define-values (directory project build!) (init-from-user-path path))
       (with-handlers ([exn:fail? log-exn])
         (build!))))))
