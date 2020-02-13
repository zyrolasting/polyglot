#lang scribble/manual

@require[@for-label[racket polyglot] "../macros.rkt"]

@title[#:tag "incorporate-se"]{How-To: Include Module Instantiation Side-Effects As Content}
In some cases you may need to capture output before incorporating it into the
page.

@section{Make the Functional Workflow Capture Content Like The Imperative Workflow}

In @(functional-workflow), you can write an app element that, like in
@(imperative-workflow), @racket[read]s tagged X-expressions
written by another module as a side-effect of instantiation:

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot racket/port)
(provide replace-page)

(define (replace-page page-tx)
  (tx-replace-me page-tx
                 (λ (x)
                   (define-values (i o) (make-pipe))
                   (parameterize ([current-output-port o])
                     (dynamic-require "loud-module.rkt" #f)
                     (close-output-port o))
                   (port->list read i))))]

@section{Make the Imperative Workflow Reformat Invalid Content}

In @(imperative-workflow), app elements cannot @racket[display]
or @racket[print] content if it would result in reading a value
that is not a Tagged X-Expression.

You can use the same trick above to capture and reformat content:

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require racket/port)

(define-values (i o) (make-pipe))
(parameterize ([current-output-port o])
  (dynamic-require "displays-string.rkt" #f)
  (close-output-port o))

(write `(p ,(port->string i)))
]
