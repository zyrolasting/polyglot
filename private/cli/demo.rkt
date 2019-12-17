#lang racket/base

(provide demo)
(require
  racket/class
  racket/file
  unlike-assets/logging
  "../../main.rkt"
  "../../paths.rkt"
  "shared.rkt")

(define (demo)
  (define compiler (new polyglot/imperative%))
  (define asset (polyglot-rel "README.md"))
  (make-directory* (dist-rel))

  (with-report/void
    (λ _ (with-handlers ([exn:fail? log-exn])
         (send compiler add! asset)
         (send compiler compile!)))))
