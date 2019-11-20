#lang racket/base

(define-syntax-rule (r+p spec ...)
  (begin
    (begin
      (require spec)
      (provide (all-from-out spec)))
    ...))

(r+p "./base.rkt"
     "./txexpr.rkt"
     "./paths.rkt"
     "./imperative.rkt"
     "./functional.rkt"
     "./projects.rkt")
