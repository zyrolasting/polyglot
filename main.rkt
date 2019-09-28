#lang racket/base

(require racket/contract)
(provide polyglot%
         discover-dependencies
         apply-manifest
         path-el/c
         polyglot-project-directory
         polyglot-rel
         project-rel
         assets-rel
         dist-rel
         system-temp-rel
         (contract-out
          [run-txexpr! (-> (or/c (non-empty-listof txexpr?) txexpr?)
                           (non-empty-listof txexpr?))]))

(require
  racket/path
  racket/class
  unlike-assets
  unlike-assets/policy
  txexpr
  "./private/fs.rkt"
  "./private/paths.rkt"
  "./private/racket-as-asset.rkt"
  "./private/default-file-handling.rkt"
  "./private/rkdown/dependencies.rkt"
  "./private/rkdown/compiler.rkt")

(define polyglot%
  (class* unlike-compiler% () (super-new)
      (define/override (delegate path)
        (case (path-get-extension path)
          [(#".md") markdown->dependent-xexpr]
          [(#".rkt") delegate-to-asset-module]
          [else copy-hashed]))

      (define/override (clarify unclear)
        (define path (build-complete-simple-path unclear (assets-rel)))
        (unless (file-readable? path) (error (format "Cannot read ~a" unclear)))
        path)))
