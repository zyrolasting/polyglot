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
    #:args (dir)
    (polyglot-project-directory (path->complete-path (simplify-path dir)))
    (define compiler (make-compiler))
    (report+summary (λ _
      (clear-distribution!)
      (send compiler add! (send compiler clarify "index.md"))
      (with-handlers ([exn:fail? log-exn])
        (send compiler compile!))))))
