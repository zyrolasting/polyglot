#lang scribble/manual
@require[@for-label[polyglot txexpr]]

@title{Build Websites Using Any Racket-Powered Language}
@author{Sage Gerard}
@defmodule[polyglot]

@racket[polyglot] lets you express web content using any Racket language among Markdown
or @seclink["top" #:doc '(lib "txexpr/scribblings/txexpr.scrbl") "tagged X-expressions"].
All websites built with @racket[polyglot] get cache-busting and Webpack-like dependency
handling for free.

@racket[polyglot] ships with a CLI to create arbitrarily rich websites and publish them to S3
using a default workflow. If you do not agree with that workflow, there's an API to write
your own.

@table-of-contents[]
@include-section["setup.scrbl"]
@include-section["paths.scrbl"]
@include-section["rackdown.scrbl"]
@include-section["default-workflow.scrbl"]
@include-section["your-workflow.scrbl"]
@include-section["txexpr.scrbl"]

@section{License and contributions}

@racket[polyglot] uses the MIT license. @hyperlink["https://github.com/zyrolasting/polyglot/blob/master/README.md" "Here's the source code."]

I always welcome contributions and feedback. If for any reason there is a problem with the name
or license, reach out to me and the matter will be resolved immediately.
