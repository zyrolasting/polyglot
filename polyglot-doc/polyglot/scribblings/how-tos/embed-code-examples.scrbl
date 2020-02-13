#lang scribble/manual

@require[@for-label[racket polyglot] "../macros.rkt"]

@title[#:tag "embed-code-examples"]{How-To: Embed Working Code Examples Using App Element Loops}

Built-in workflows will not consider a page "done" until all app elements
and lib elements are gone. You can leverage this to iterate on content.
Obviously this comes with a risk of an infinite loop, but that's loops for
you.

One useful example is in showing code to the end-user, plus the actual
output of the same code.

@section{Imperative Workflow}

In @(imperative-workflow), all you need to do to make a working code example
is print the code in a preformatted block along with a new app element
containing the same code. It's fine if each line of code is a seperate
child node of the script element. The lines will be joined with newline
characters internally.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base
(define code '("#lang racket"
               "(write `(h1 \"Hello, meta.\"))"))

(write `(pre . ,code))
(write `(script ((id "example") (type "application/racket")) . ,code))
]

@section{Functional Workflow}

An equivalent working code example for the Functional Workflow is as follows:

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot)
(provide replace-page)

(define code '("#lang racket/base"
               "(provide replace-page)"
               "(require polyglot)"
               "(define (replace-page page-tx)"
               "  (tx-replace-me page-tx"
               "              (λ (x) '((h1 \"Hello, meta!\")))))"))

(define (replace-page page-tx)
  (tx-replace-me page-tx
                 (λ (x) `((pre . ,code)
                          (script ((type "application/racket")) . ,code)))))]
