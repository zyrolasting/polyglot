#lang scribble/manual
@require[@for-label[polyglot unlike-assets]]

@title{Workflows}

A @tech{project}'s @deftech{workflow} reads files from the project's
@tech{assets directory} and writes files to the project's
@tech{distribution directory}.

A workflow's implementation is a subclass of
@racket[unlike-compiler%], which allows any asset to depend on other
assets. Any workflow class is a valid value for @racket[polyglot+%] in
@tt{.polyglotrc.rkt}.

@include-section{built-in-workflows.scrbl}
@include-section{txexpr.scrbl}
@include-section{elements.scrbl}
