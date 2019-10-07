#lang scribble/manual
@require[@for-label[polyglot
                    txexpr
                    unlike-assets
		    racket/rerequire
                    racket/base
                    racket/dict
                    racket/class]]

@title[#:tag "default-workflow"]{The Imperative Workflow}
@defmodule[polyglot/imperative]

The imperative workflow has a PHP-like development experience where
you can "drop into" an app element and use any @tt{#lang} to write
content to appear in place. The workflow accepts mixed-mode Markdown
as input and writes HTML5 pages to @racket[dist-rel] as output.

This approach is intuitive and works naturally with @tt{#lang}s
that print data as a side-effect. The drawback is that this workflow
does not scale well, and requires an increasingly noisy API to handle
corner cases. If you want a leaner, more adaptable
workflow with less room for error, use @secref{functional-workflow}.

This is the default workflow, meaning that it applies when no
workflow is specified according to @secref{setup}.

@section[#:tag "imperative-apps"]{Imperative App and Library Elements}

Imperative app elements use @secref["printing" #:doc '(lib "scribblings/reference/reference.scrbl")] in @racket[write] mode to emit tagged X-Expressions. The zero or more written elements will replace the printing script element in the internal @racket[txexpr] representing the page.

@verbatim[#:indent 2]|{
# Hello, world

<script type="application/racket" id="main">
#lang racket/base
(require racket/format racket/date)

(write `(p ,(format "Today is ~a" (date->string (current-date)))))
</script>
}|

Two notes of caution:
@itemlist[
@item{Don't use @racket[print] or @racket[display] unless you know what
will happen. The imperative workflow uses @racket[read] internally with
the intent of building a list of tagged X-expressions.}
@item{To avoid confusion or unwanted output, either avoid using the printer in the
top-level of library element code or capture any output produced as a side-effect of
instantiating a library element's module.}]

@subsection[#:tag "imperative-layouts"]{Setting a Page Layout}
Application elements may use @racket[(provide layout)] to set a layout for a page.

@verbatim[#:indent 2]|{
# Hello, world

<script type="application/racket" id="main">
#lang racket/base

(provide layout)
(require racket/format racket/date)

(write `(p ,(format "Today is ~a" (date->string (current-date)))))

(define (layout kids)
  `(html (head (link ((rel "stylesheet") (href "styles.css")))
               (title "My page"))
	 (body . , kids)))
</script>
}|

This produces:

@verbatim[#:indent 2]|{
<!DOCTYPE html>
<html>
  <head>
    <link rel="stylesheet" href="styles.css" />
    <title>My page</title>
  </head>
  <body>
    <h1>Hello, world</h1>
    <p>Today is Friday, September 20th, 2019</p>
  </body>
</html>
}|

If multiple application elements in a page each provide a layout,
@racket[polyglot] will use the last layout specified.

@subsection[#:tag "gen-app-elements"]{Generating Imperative App Elements}
The imperative workflow will continue processing content until no
application elements remain. You can leverage this to generate
application elements using another application element. One use case
is creating code samples paired with actual output embedded in
the page.

@racketblock[
(define code '("#lang racket"
               "(write `(h1 \"Hello, meta.\"))"))

(write `(pre . ,code))
(write `(script ((id "example") (type "application/racket")) ,@code))
]

If a build generates infinitely many application elements, then it will not terminate.


@include-section["macros.scrbl"]

@section{Imperative Workflow API Reference}
@defclass[polyglot/imperative% polyglot/base% ()]{
Implements the workflow documented on this page.

@defmethod[(delegate [clear clear/c]) unlike-asset/c]{
This implements the imperative workflow by extending @secref{base-workflow}.
}

@defmethod[(preprocess-txexprs [tx-expressions (listof txexpr?)]) (listof txexpr?)]{
This method transforms @racket[tx-expressions] parsed from a source Markdown file
into a new list of tagged X-expressions. This transformation occurs before processing
any script elements.

Use this to sanitize untrusted code, generate application elements based on content,
or attach common metadata to documents.
}
}

@defproc[(run-txexpr/imperative! [tx-expressions (or/c txexpr? (non-empty-listof txexpr?))]
                                 [initial-layout (-> (listof txexpr?) (or/c txexpr? (listof txexpr?)))
                                                 identity])
                                 (or/c (listof txexpr?) txexpr?)]{
Dynamically evaluate the Racket modules in @racket[tx-expressions] according to
the rules of this workflow, without using the CLI or @racket[polyglot/imperative%].

Note that you do not have to use Markdown before using this function, or even
tagged expressions that represent valid HTML. You only need to represent
application or library @tt{<script>} elements as tagged X-expressions.

This wil replace Racket application and library elements inside the
provided tagged X-Expressions and return the expanded expressions within a layout.

Remember from @secref{imperative-layouts} that the application elements within the
@racket[tx-expressions] are the authority of their own layout, but you may specify
an @racket[initial-layout] that will apply if the application elements don't supply one.

This procedure has side-effects:

@itemlist[
@item{Unique and short-lived directories and Racket modules will appear in @racket[(system-temp-rel)] according to @secref{rackdown}. They are deleted by the time control leaves this procedure.}
@item{Events will appear on @racket[unlike-assets-logger]. Info-level events will report summarized content fragments created by your application elements.  Any output on @racket[(current-error-port)] caused by script elements will apperar as error-level events.}
]

Note that this procedure does not do anything with dependencies.
Use @racket[discover-dependencies] to analyze the output and
find more assets to process.}

@defthing[polyglot% class?]{
An alias for @racket[polyglot/imperative%] kept for backwards compatibility.}
@defthing[run-txexpr! procedure?]{
An alias for @racket[run-txexpr/imperative!] kept for backwards compatibility.}
