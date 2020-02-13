#lang scribble/manual
@require[@for-label[racket polyglot unlike-assets]]

@title{Built-In Workflows}

To save time for new users, Polyglot ships with three built-in
workflows: @racket[polyglot/base%], @racket[polyglot/imperative%], and
@racket[polyglot/functional%]. Each may be extended, overridden, or
ignored entirely.

@racket[polyglot/imperative%] and @racket[polyglot/functional%] both
operate under the assumption that they must produce HTML5 documents
from Markdown files. In addition, each Markdown file may contain
@deftech{application elements} and @deftech{library elements}.
Application elements are of form @litchar{<script
type="application/racket">...</script>}, and library elements are of form
@litchar{<script type="text/racket">...</script>}. The text content of
these elements are copied to Racket modules contained in
@racket[(polyglot-temp-directory)]. The name of each Racket module is
equal to @litchar{ID.rkt}, where @litchar{ID} is the value of the
@litchar{id} attribute of the @litchar{<script>} element. If the
@litchar{id} attribute is not present or empty, a temporary name will
be used instead.

The Racket modules created on disk from application elements are
loaded using @racket[dynamic-require] in the order they are
encountered in Markdown. The intended behavior and responsibility
of application element's Racket module depends on the workflow
executing it. Library elements, on the other hand, are saved to disk
as-is and used according to the whims of the Racket modules produced
from application elements.

@include-section{base.scrbl}
@include-section{imperative.scrbl}
@include-section{functional.scrbl}
