#lang racket/base

(provide start)
(require racket/cmdline
         "../../main.rkt"
         "../fs.rkt"
         "./shared.rkt")

(define (start)
  (define skel (make-parameter "imperative"))
  (command-line
   #:program "start"
   #:once-each
   [("-f" "--functional")
    "Set up project to use the functional workflow"
    (skel "functional")]
   #:args (dir)
    (copy-skeleton (skel) dir)))
