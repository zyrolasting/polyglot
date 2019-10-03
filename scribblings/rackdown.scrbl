#lang scribble/manual
@require[@for-label[polyglot
                    txexpr
                    racket/base]]

@title[#:tag "rackdown"]{Mixed-mode Racket}

The workflows shipped with @racket[polyglot] target HTML @tt{<script>}
elements in the form of tagged X-expressions. We'll focus on
the rules for those script elements here, and discuss how those
rules apply depending on your workflow.

There are two kinds of @tt{<script>} elements: @defterm{library ("lib") elements}
and @defterm{application ("app") elements}. @racket[polyglot] processes them in that
order, but we will cover application elements first.

Each app or lib element holds the source code for a Racket module as CDATA,
including the @tt{#lang} line. The @tt{src} attribute on these @tt{<script>}
elements are ignored, because they contribute complexity without added benefit in
the context of @racket[polyglot]. The @racket[src] attributes on all other script
elements work according to a given workflow.

@tt{<script id="foo" ...>} causes @racket[polyglot] to write the CDATA of
that script element to @tt{<tmp>/foo.rkt}, where @tt{<tmp>} is a
temporary directory created inside @racket[(system-temp-rel)]. If you do
not specify a value for an @tt{id} attribute, @racket[polyglot] will
generate one for you.

@margin-note{On Windows, your account might not have permission to create links. Either run @racket[polyglot] as an Administrator, or (better yet) get permission to create links.}
Since the Racket modules exist in the same directory when processed together,
they can @racket[require] each other. @racket[polyglot] also leaves a symlink
named @tt{project} pointing to your project directory so that
multiple pages can share code and data.

Be careful to include any Racket-level dependencies in @tt{info.rkt} or a
setup script.

@section{Relationship to Markdown}

@racket[polyglot] includes workflows that accept Markdown as input. Markdown
is not required for custom workflows, but it helps to understand why it appears.
Examples in this documentation will use Markdown since they can be built using
the @secref{cli}.

Markdown supports use of HTML tags, so we can just write the @tt{<script>} elements
we want directly among prose. This gives a nice experience where you can "drop into"
any DSL to work on the underlying page model when plain language won't cut it.

Clearly out of a fit of inspiration and unrestrained genius, we'll call the
mix of Racket and Markdown @defterm{Rackdown}. To be clear, Rackdown isn't special.
Any Markdown parser can parse pages written for @racket[polyglot]'s workflows.

@section{Application Elements}
@margin-note{@racket["application/rackdown"] is also accepted for backwards compatibility.}

@defterm{application elements} are HTML script elements of media type
@racket["application/racket"]. @racket[polyglot] will run them in the
order they are encountered. The way the application elements run
depends on the workflow you use.

Here's an example from @secref{default-workflow}. In this workflow, app
elements @racket[write] tagged X-expressions to replace the surrounding
app element.

@verbatim[#:indent 2]|{
# Hello, world

<script type="application/racket" id="main">
#lang racket/base
(require racket/format racket/date)

(write `(p ,(format "Today is ~a" (date->string (current-date)))))
</script>
}|

This would produce something like:

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

Here's an equivalent example from @secref{functional-workflow},
where an app element can provide a procedure to replace the
entire page.

@verbatim[#:indent 2]|{
# Hello, world

<script type="application/racket" id="main">
#lang racket/base
(provide replace-page)
(require racket/format
         racket/date
         polyglot)

(define (replace-page page)
  (replace page
           (current-app-element-predicate)
           `((p ,(format "Today is ~a" (date->string (current-date)))))))
</script>
}|


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
script (removing it from the output HTML) and write it to @tt{<tmp>/components.rkt}.
It's especially important to name your libraries with an @racket[id] attribute, or else
you will not know what name it has on disk in an application element.


@section{Accessing shared content}

Library elements provide shared content @italic{within} a page.
To share code and data @italic{across} pages, place related files
in your project directory and use the @racket["project"] symlink
to access them.

@verbatim[#:indent 2]|{
<script type="text/racket" id="main">
#lang racket/base
(require "project/components.rkt")
(provide (rename-out [two-column layout]))
</script>
}|
