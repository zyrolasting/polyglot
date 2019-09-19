#lang racket/base

(provide (all-defined-out))
(require racket/contract
         unlike-assets
         unlike-assets/logging)

(define/contract (delegate-to-asset-module clear compiler) advance/c
  (<info "Delegating to ~a's write-dist-file" clear)
  (dynamic-require clear 'write-dist-file))
