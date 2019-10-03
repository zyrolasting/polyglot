#lang racket/base

(require racket/contract)
(provide (rename-out [polyglot/imperative% polyglot%])
         polyglot/imperative%
         discover-dependencies
         apply-manifest
         path-el/c
         polyglot-project-directory
         polyglot-temp-directory
         polyglot-rel
         project-rel
         assets-rel
         dist-rel
         system-temp-rel
         (contract-out
          [run-txexpr! (-> (or/c (non-empty-listof txexpr?) txexpr?)
                           (non-empty-listof txexpr?))]))

(require
  racket/class
  racket/path
  racket/rerequire
  racket/string
  unlike-assets
  unlike-assets/logging
  unlike-assets/policy
  "./txexpr.rkt"
  "./private/fs.rkt"
  "./private/paths.rkt"
  "./private/racket-as-asset.rkt"
  "./private/default-file-handling.rkt"
  "./private/dependencies.rkt"
  "./workflows/imperative.rkt")
