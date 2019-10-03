#lang scribble/manual
@require[@for-label[polyglot txexpr]]

@title{Build Websites Using Any Racket-Powered Language}
@author{Sage Gerard}
@defmodule[polyglot]

@racketmodname[polyglot] is a collection of workflows and libraries that
evaluate Racket modules within @seclink["top" #:doc '(lib
"txexpr/scribblings/txexpr.scrbl") "tagged X-expressions"]. It includes
bindings from @racketmodname[polyglot/paths], @racketmodname[polyglot/txexpr],
@racketmodname[polyglot/imperative], and @racketmodname[polyglot/functional].

@racketmodname[polyglot] ships with a CLI that, by default, creates
rich websites using a Markdown/Racket mix. All websites built with this
workflow get cache-busting, Webpack-like dependency handling, and an S3
upload tool for free.

And if you do not agree with any workflow, there's an API to write your own.

@table-of-contents[]
@include-section["setup.scrbl"]
@include-section["paths.scrbl"]
@include-section["workflows.scrbl"]
@include-section["cli.scrbl"]
@include-section["default-workflow.scrbl"]
@include-section["functional-workflow.scrbl"]
@include-section["your-workflow.scrbl"]

@section{License and contributions}
@racket[polyglot] uses the MIT license. @hyperlink["https://github.com/zyrolasting/polyglot/blob/master/README.md" "Here's the source code."]

I always welcome contributions and feedback. If for any reason there is a
problem with the name or license, reach out to me and the matter will be
resolved immediately.
