#lang scribble/manual
@require[@for-label[
  polyglot
  txexpr
  unlike-assets
  racket/base]]

@title[#:tag "your-workflow"]{Writing Custom Workflows}

If the workflows included in the collection do not suit your needs,
then you can write your own.

@section[#:tag "extending"]{Extending @racket[polyglot] classes}
To avoid doing too much work you can just extend or specialize an existing
workflow. You'll need some familiarity with @seclink["top" #:doc '(lib
"unlike-assets/scribblings/unlike-assets.scrbl")
"unlike-assets"].

Each workflow has a documented class. If you subclass it, that subclass represents a
new workflow.

You can use the workflow programmatically according to the documentation for @racket[unlike-compiler%].

If you want to integrate your class with the polyglot CLI, write a module
that uses @racket[(provide polyglot+%)] to export your class definition.

@racketmod[#:file "my-polyglot.rkt"
racket/base
(require racket/class polyglot)
(provide polyglot+%)
(define polyglot+% (class* polyglot% () (super-new) #;...))
]

Then, using the CLI, specify your module after
the @tt{polyglot} command, but before the
subcommand.

@verbatim[#:indent 2]|{
$ raco polyglot -b my-polyglot.rkt build my-site
}|

@include-section["txexpr.scrbl"]
