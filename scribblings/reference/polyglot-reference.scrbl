#lang scribble/manual
@require[@for-label[polyglot]]

@title{Polyglot: API Reference}
@author{Sage Gerard}

@defmodule[polyglot]

This collection configures @racketmodname[unlike-assets] to build
deeply-integrated websites using programs embedded in Tagged
X-Expressions.

For a gentle introduction to Polyglot and its benefits, see
@other-doc['(lib
"polyglot/scribblings/guide/polyglot-guide.scrbl")]. For tutorials on
specific topics beyond the guide, see @other-doc['(lib
"polyglot/scribblings/tutorials/polyglot-tutorials.scrbl")]. For
recipes used to carry out specific tasks, see @other-doc['(lib
"polyglot/scribblings/how-tos/polyglot-how-tos.scrbl")].

The @racketmodname[polyglot] module includes bindings from
@racketmodname[polyglot/builds], @racketmodname[polyglot/paths],
@racketmodname[polyglot/txexpr], @racket[polyglot/elements],
@racketmodname[polyglot/projects], @racketmodname[polyglot/base],
@racketmodname[polyglot/imperative], and @racketmodname[polyglot/functional].

@table-of-contents[]
@include-section{projects-index.scrbl}
@include-section{workflows.scrbl}
@include-section{applying.scrbl}
@include-section{contributions.scrbl}
