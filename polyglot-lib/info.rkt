#lang info

(define raco-commands
  '(("polyglot" (submod polyglot/private/cli/entry main) "polyglot CLI" #f)))
(define racket-launcher-libraries '("private/cli/entry.rkt"))
(define racket-launcher-names '("polyglot"))

(define compile-omit-paths '("private/skel"))
