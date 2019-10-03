#lang racket/base

(define-syntax-rule (r+p spec ...)
  (begin
    (begin
      (require spec)
      (provide (all-from-out spec)))
    ...))

(r+p "./txexpr.rkt"
     "./paths.rkt"
     "./imperative.rkt"
     "./functional.rkt")
