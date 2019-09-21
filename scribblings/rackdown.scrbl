#lang scribble/manual
@require[@for-label[polyglot
                    txexpr
                    racket/base]]

@title[#:tag "rackdown"]{Mixed-mode Racket}

@racket[polyglot]'s treats HTML @racket[<script>] elements (in the form of tagged X-expressions)
as if they were part of a broader Racket project.

There are two kinds of @racket[<script>] elements: @defterm{library elements}
and @defterm{application elements}. @racket[polyglot] processes them in that
order, but we will cover application elements first.

Each element holds the source code for a Racket module as CDATA. The @racket[src]
attribute on these @racket[<script>] elements are ignored, because they contribute
complexity without added benefit in the context of @racket[polyglot]. However,
the @racket[src] attributes on all other script elements work according to
the @secref{default-workflow} section.

@racket[<script id=foo ...>] causes @racket[polyglot] to write the CDATA of
that script element to @racket["<tmp>/foo.rkt"], where @racket["<tmp>"] is a
temporary directory created inside @racket[(system-temp-rel)]. If you do
not specify a value for an @racket[id] attribute, @racket[polyglot] will
generate one for you.

@margin-note{On Windows, your account might not have permission to create links. Either run @racket[polyglot] as an Administrator, or (better yet) get permission to create links.}
Since the Racket modules exist in the same directory when processed together,
they can @racket[require] each other. @racket[polyglot] also leaves a symlink
named @racket["<tmp>/project"] pointing to your project directory so that
multiple pages can share code and data.

Be careful to include any Racket-level dependencies in @racket[info.rkt] or a
setup script, since they will not be as easy to detect using static analysis.

@section{Relationship to Markdown}

Markdown supports use of HTML tags, so we can just write the @racket[<script>] elements
we want directly among prose. This gives a nice experience where you can "drop into"
any DSL to work on the underlying page model when plain language won't cut it.

Clearly out of a fit of inspiration and unrestrained genius, we'll call the
mix of Racket and Markdown @defterm{Rackdown}. To be clear, Rackdown isn't special.
Any Markdown parser can parse pages written for @racket[polyglot]'s default workflow.

@racket[polyglot]'s handling of scripts and Markdown are decoupled.
If you write your own workflow, you do not @italic{have} to use Markdown.
Here I assume you are using Markdown for simplicity, and to illustrate
how script processing works with Markdown examples. All of these examples
work with @racket[raco polyglot build].

@section{Application Elements}

@margin-note{@racket["application/rackdown"] is also accepted for backwards compatibility.}

@defterm{application elements} are HTML script elements of media type @racket["application/racket"]. @racket[polyglot] will run them as they are encountered, in order from the beginning of the page to the end.

@verbatim[#:indent 2]|{
# Hello, world

<script type="application/racket" id="main">
#lang racket/base
(require racket/format racket/date)

(write `(p ,(format "Today is ~a" (date->string (current-date)))))
</script>
}|

In the default workflow, this would produce something like:

@verbatim[#:indent 2]|{
<!DOCTYPE html>
<html>
  <head>
    <title>Untitled</title>
  </head>
  <body>
    <h1>Hello, world</h1>
    <p>Today is Friday, September 20th, 2019</p>
  </body>
</html>
}|

Application elements uses @secref["printing" #:doc '(lib "scribblings/reference/reference.scrbl")] in @racket[write] mode to emit tagged X-Expressions. The zero or more written elements will replace the one script element in the internal @racket[txexpr] representing the page.

Don't use @racket[print] or @racket[display] unless you know what @racket[polyglot] would @racket[read] and place in your page.


@subsection{Setting a Page Layout}

Application elements may use @racket[(provide layout)] to set a layout for a page.
@racket[polyglot] avoids the complexity of templating engines by
letting content place itself within another @racket[txexpr].

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


@subsection{Generating Application Elements}

@racket[polyglot] will continue processing content until no
application elements remain. You can leverage this to generate
application elements within an application element. One use case
is creating code samples paired with actual output embedded in
the page.

@racketblock[
(define code '("#lang racket"
               "(write `(h1 \"Hello, meta.\"))"))

(write `(pre . ,code))
(write `(script ((id "example") (type "application/racket")) ,@code))
]

If a build generates infinitely many application elements, then it will not terminate.


@section{Library Elements}

Library elements have media type @racket["text/racket"]. Library elements are saved
to disk before application elements run. This lets you share code and data within
the page.

@verbatim[#:indent 2]|{
# Available Packages

<script type="text/racket" id="components">
#lang racket/base

(provide offering)

(define (offering label price features)
  `(section
    (h3 ,label)
    (h4 ,price)
    (ul ,(map (lambda (v) `(li ,v)) features))))
</script>


## Personal use

<script type="application/racket">
#lang racket/base
(require "components.rkt")

(write (offering "Basic" "FREE"))
(write (offering "Plus" "$10/month"))
(write (offering "Pro" "$20/month"))
</script>


## Enterprise

<script type="application/racket">
#lang racket/base
(require "components.rkt")

(write (offering "Basic" "$100/month"))
(write (offering "Plus" "$500/month"))
(write (offering "Pro" "$1,000/month"))
</script>
}|

In this example @racket[polyglot] will consume the @racket["text/racket"]
script (removing it from the output HTML) and write it to @racket["<tmp>/components.rkt"].
It's especially important to name your libraries with an @racket[id] attribute, or else
you will not know what name it has on disk in an application element.

A Racket module created from a library element will not be visited or instantiated
until an application element uses it. To avoid confusion or unwanted output,
either avoid using the printer in the top-level of library element code or capture
any output produced as a side-effect of instantiating a library element's module.


@section{Accessing shared content}

Library elements provide shared content @italic{within} a page.
To share code and data @italic{across} pages, place related files
in your project directory and use the @racket["project"] symlink
to access them.

You'll need this to avoid duplicating layout code, for example:

@verbatim[#:indent 2]|{
<script type="application/racket" id="main">
#lang racket/base
(require "project/components.rkt")
(provide (rename-out [two-column layout]))
</script>
}|
