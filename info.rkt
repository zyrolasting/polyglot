#lang info
(define collection "polyglot")
(define deps '("base" "file-watchers" "txexpr" "unlike-assets" "aws" "markdown"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "Build websites with any mix of programming languages among prose.")
(define scribblings '(("scribblings/guide/polyglot-guide.scrbl" (multi-page) (tools))
                      ("scribblings/reference/polyglot-reference.scrbl" (multi-page) (library))
                      ("scribblings/how-tos/polyglot-how-tos.scrbl" (multi-page) (omit-start))
                      ("scribblings/tutorials/polyglot-tutorials.scrbl" (multi-page) (omit-start))))
(define version "2.4")
(define pkg-authors '(sage))
(define raco-commands
  '(("polyglot" (submod polyglot/private/cli/entry main) "polyglot CLI" #f)))
(define racket-launcher-libraries '("private/cli/entry.rkt"))
(define racket-launcher-names '("polyglot"))

(define test-omit-paths '("scribblings"))
(define compile-omit-paths '("private/skel"))
