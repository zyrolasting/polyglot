#lang info
(define collection 'multi)
(define deps '("aws"
               "base"
               "css-expr"
               "file-watchers"
               "markdown"
               "polyglot-lib"
               "txexpr"
               "https://github.com/zyrolasting/unlike-assets.git?path=unlike-assets-lib#7fb08d6902d34399c96a325d68fd27f1ff84813b"
               "https://github.com/zyrolasting/unlike-assets.git?path=unlike-assets-doc#7fb08d6902d34399c96a325d68fd27f1ff84813b"))
(define build-deps '("scribble-lib" "racket-doc"))
(define pkg-desc "Documentation for Polyglot")
(define update-implies '("polyglot-lib"))
(define pkg-authors '(sage))
