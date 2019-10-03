#lang racket/base

(provide start)
(require
  racket/cmdline
  racket/file
  "../fs.rkt"
  "../../paths.rkt")

(define (start)
  (command-line
    #:program "start"
    #:args (dir)
    (copy-directory/files (polyglot-rel "skel") dir)))
