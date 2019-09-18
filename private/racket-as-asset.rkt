#lang racket/base

(provide (all-defined-out))
(require racket/contract
         unlike-assets)

(define/contract (delegate-to-asset-module clear compiler) advance/c
  (dynamic-require clear 'advance))
