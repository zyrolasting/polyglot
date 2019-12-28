#lang info

(define collection 'multi)

(define pkg-desc "Polyglot Documentation")
(define pkg-authors '(sage))

(define deps '("base"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "css-expr"
                     "polyglot-lib"))

(define update-implies '("polyglot-lib"))
