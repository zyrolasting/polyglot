#lang scribble/manual
@require[@for-label[polyglot unlike-assets]]

@title{Dependency Discovery and Processing}

Once there are no more app or lib elements to process in
a Markdown file, a workflow will check all @racket[href]
and @racket[src] attribute values on a page and keeps the ones that look
like assets on your disk (including @racket["file:"] URLs).

These values are @bold{either absolute paths on your filesystem, or
paths relative to your assets directory} (See @racket[assets-rel]).
So if there is a link to @tt{contact.md} somewhere on your page,
a built-in workflow will try to process that file from @racket[(assets-rel "contact.md")].

For each dependency, your chosen workflow will react according
to that dependency's type.