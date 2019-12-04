#lang scribble/manual
@require[@for-label[polyglot
                    txexpr
                    unlike-assets
		    racket/rerequire
                    racket/base
                    racket/dict
                    racket/class]]

@title[#:tag "imperative-workflow"]{The Imperative Workflow}

The imperative workflow has a PHP-like development experience where you can
"drop into" Racket code and use any @litchar{#lang} to write content to appear
in place. The workflow accepts mixed-mode Markdown as input and writes HTML5
pages to @racket[dist-rel] as output.

This approach is intuitive and works naturally with @tt{#lang}s that print data
as a side-effect. The drawback is that this workflow does not scale well, and
requires an increasingly noisy API to handle corner cases. If you want a
leaner, more adaptable workflow with less room for error, use
@secref{functional-workflow}.

This is the default workflow for backwards-compatibility reasons, meaning that
it applies when no workflow is specified when running @tt{polyglot} commands.

@section[#:tag "imperative-apps"]{Imperative App and Library Elements}

Imperative app elements use @secref["printing" #:doc '(lib
"scribblings/reference/reference.scrbl")] in @racket[write] mode to emit tagged
X-Expressions. The zero or more written elements will replace the printing
script element in the internal @racket[txexpr] representing the page.

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
@item{Don't use @racket[print] or @racket[display] unless you know what will
happen. The imperative workflow uses @racket[read] internally with the intent
of building a list of tagged X-expressions.}
@item{To avoid confusion or unwanted output, either avoid using the printer in
the top-level of library element code or capture any output produced as a
side-effect of instantiating a library element's module.}]

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

If multiple application elements in a page each provide a layout, the
imperative workflow will use the last layout specified.

@include-section["macros.scrbl"]
