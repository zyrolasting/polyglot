#lang scribble/manual
@require[@for-label[polyglot unlike-assets]]

@title{Workflows}

@margin-note{This distinction is important: If @racket[polyglot] viewed tagged X-expressions
as programs, then it would resemble a limited and redundant Lisp.}
In the context of this collection, a @defterm{workflow} is a subjective
way of working with tagged X-expressions as containers of Racket modules.
Workflows do @italic{not} view tagged X-expressions as programs themselves.

Each workflow is implemented as one subclass of @racket[unlike-compiler%].
This collection ships with two workflows, and therefore two subclasses:
@secref{default-workflow}, and @secref{functional-workflow}.

@racket[polyglot]'s CLI allows you to select one of the pre-built workflows,
or specify your own subclass to use instead (See @secref{your-workflow}).

In this section we'll cover the rules and tools available to all workflows.

@include-section["rackdown.scrbl"]
@include-section["asset-handling.scrbl"]
