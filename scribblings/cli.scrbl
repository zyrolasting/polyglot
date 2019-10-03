#lang scribble/manual
@require[@for-label[polyglot
                    racket/base]]

@title[#:tag "cli"]{Command Line Interface}

The command line interface offers the means to
develop and publish your project using a chosen
workflow.

@section{The @tt{build} command}

@verbatim[#:indent 2]|{
$ raco polyglot build PROJECTPATH
}|

@itemlist[
#:style 'ordered
@item{Set @racket[polyglot-project-directory] to @tt{PROJECTPATH}.}
@item{Determine the workflow to use.
      @itemlist[
      @item{If no workflow is specified, use @secref{default-workflow} by loading @racket[polyglot/imperative%].}
      @item{If @tt{-f} is set, use @secref{functional-workflow} by loading @racket[polyglot/functional%].}
      @item{If @tt{-b MODULE} is set, use the workflow specified by the class returned from @racket[(dynamic-require MODULE 'polyglot+%)]}]}
@item{Add @racket[(assets-rel "index.md")] to the compiler.}
@item{Process @tt{index.md} along with any dependencies according to the workflow.}
]

@section[#:tag "live"]{Live Builds}

Use the @racket[develop] command to rebuild your website in response to changes in assets.

@verbatim[#:indent 2]|{
$ raco polyglot develop my-website
}|

@include-section["publishing.scrbl"]