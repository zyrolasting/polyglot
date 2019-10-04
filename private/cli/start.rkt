#lang racket/base

(provide start)
(require racket/class
         racket/cmdline
         racket/file
         "../../main.rkt"
         "../fs.rkt"
         "./shared.rkt")

(define (start)
  (command-line #:program "start" #:args (dir)
    (copy-skeleton (if (is-a? (make-compiler) polyglot/functional%)
                       "functional"
                       "imperative")
                   dir)))
