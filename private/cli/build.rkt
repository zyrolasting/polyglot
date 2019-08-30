#lang racket/base

(provide build)
(require
  racket/cmdline
  racket/file
  racket/class
  unlike-assets/logging
  unlike-assets
  "../../main.rkt"
  "../fs.rkt"
  "../paths.rkt"
  "shared.rkt")


(define (build)
  (define compiler (new polyglot%))
  (command-line
    #:program "build"
    #:args (dir)
    (polyglot-project-directory (path->complete-path (simplify-path dir)))
    (report+summary (λ _
      (make-directory* (dist-rel))
      (empty-directory (dist-rel))
      (send compiler add! (send compiler clarify "index.md"))
      (send compiler compile!)))))
