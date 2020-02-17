#lang info
(define collection 'multi)
(define deps '("aws"
               "base"
               "css-expr"
               "file-watchers"
               "markdown"
               "polyglot-lib"
               "txexpr"
               "unlike-assets-doc"
               "unlike-assets-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define pkg-desc "Documentation for Polyglot")
(define update-implies '("polyglot-lib"))
(define pkg-authors '(sage))
