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
  txexpr
  racket/class
  racket/path
  racket/rerequire
  racket/string
  unlike-assets
  unlike-assets/logging
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
        path)

      (define/public (preprocess-txexprs tx-expressions)
        (for/list ([tx (in-list tx-expressions)])
          (define-values (new-content _)
            (splitf-txexpr
             tx
             (λ (x)
               (and (txexpr? x)
                    (string? (attr-ref x 'data-macro #f))))
             (λ (x)
               (define modspec/list
                 (regexp-split #px"\\s+"
                               (string-trim (attr-ref x 'data-macro))))

               (define module-path
                 (assets-rel (format "~a.rkt"
                                     (car modspec/list))))

               (define provided
                 (if (> (length modspec/list) 1)
                     (string->symbol (cadr modspec/list))
                     'replace-element))

               (<debug "data-macro: load `~a` from `~a`"
                       provided
                       module-path)

               (dynamic-rerequire module-path)
               ((dynamic-require module-path provided) x))))
          new-content))))
