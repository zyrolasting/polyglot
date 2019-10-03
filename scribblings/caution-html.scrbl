#lang scribble/manual
@require[@for-label[polyglot unlike-assets]]

@title{A Cautionary Tale About Invalid HTML}

@racket[polyglot]'s Markdown parser happens to handle
invalid elements, making code golf easy. But there's a
gotcha.

Let's say you write a custom @tt{page} element to
represent your page title, layout, and keywords in
one shot.

@verbatim[#:indent 2]|{
<page title="My Page Title" layout="two-column" keywords="pizza, skateboards, cowabunga">
}|

You can build this with @racket[polyglot], and you'll get output.

Only it won't work as expected because you forgot to close the elements,
XHTML style. If you add the missing @tt{/}s, it will work.

@verbatim[#:indent 2]|{
<page title="My Page Title" layout="two-column" keywords="pizza, skateboards, cowabunga" />
}|

The Markdown parser will treat HTML5 void elements as self-closing,
but you must explicitly close anything you make up under XML rules.

If you are using the pre-built workflows, either stick to HTML5 elements
or make sure they end up as HTML5 elements. That is the document type
that the built-in workflows deliver to end-users.
