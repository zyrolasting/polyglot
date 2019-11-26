#lang info
(define collection "polyglot")
(define deps '("base" "file-watchers" "txexpr" "unlike-assets" "aws" "markdown"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "An unlike compiler that generates static websites using a Markdown and any #lang")
(define scribblings '(("scribblings/polyglot.scrbl" (multi-page))))
(define version "1.10")
(define pkg-authors '(sage))
(define raco-commands
  '(("polyglot" (submod polyglot/private/cli/entry main) "polyglot CLI" #f)))
(define racket-launcher-libraries '("private/cli/entry.rkt"))
(define racket-launcher-names '("polyglot"))
