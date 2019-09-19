#lang racket/base

(provide (all-defined-out))
(require racket/contract
         racket/rerequire
         unlike-assets
         unlike-assets/logging)

(define/contract (delegate-to-asset-module clear compiler) advance/c
  (<info "Delegating to ~a's write-dist-file" clear)
  (dynamic-rerequire clear)
  (dynamic-require clear 'write-dist-file))
