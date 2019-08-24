#lang racket/base

(provide demo)
(require
  racket/class
  racket/file
  unlike-assets/logging
  "../../main.rkt"
  "../fs.rkt"
  "../paths.rkt")

(define (demo)
  (define compiler (new polyglot%))
  (define asset (polyglot-rel "README.md"))
  (make-directory* (dist-rel))

  (with-report/void
    (λ _ (with-handlers ([exn:fail? (λ (e) (<error "~a" e))])
         (send compiler add! asset)
         (send compiler compile!)))))
