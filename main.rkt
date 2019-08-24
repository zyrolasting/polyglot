#lang racket/base

(provide polyglot%)
(require
  racket/path
  racket/class
  unlike-assets
  unlike-assets/policy
  "./private/fs.rkt"
  "./private/paths.rkt"
  "./private/default-file-handling.rkt"
  "./private/rkdown/compiler.rkt")

(define polyglot%
  (class* unlike-compiler% () (super-new)
      (define/override (delegate path)
        (case (path-get-extension path)
          [(#".md") markdown->dependent-xexpr]
          [else copy-hashed]))

      (define/override (clarify unclear)
        (define path (build-complete-simple-path unclear (assets-rel)))
        (unless (file-readable? path) (error (format "Cannot read ~a" unclear)))
        path)))
