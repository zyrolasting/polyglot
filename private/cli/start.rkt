#lang racket/base

(provide start)
(require racket/class
         racket/cmdline
         "../../main.rkt"
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
    (get-field directory (copy-polyglot-skeleton-project! (skel) dir))))
