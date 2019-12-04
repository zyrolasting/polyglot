#lang racket

(require scribble/manual)
(provide (all-defined-out))

(define-syntax-rule (base-workflow)
  (secref "base-workflow" #:doc '(lib "polyglot/scribblings/guide/polyglot-guide.scrbl")))
(define-syntax-rule (functional-workflow)
  (secref "functional-workflow" #:doc '(lib "polyglot/scribblings/guide/polyglot-guide.scrbl")))
(define-syntax-rule (imperative-workflow)
  (secref "imperative-workflow" #:doc '(lib "polyglot/scribblings/guide/polyglot-guide.scrbl")))
