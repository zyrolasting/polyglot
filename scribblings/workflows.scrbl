#lang scribble/manual
@require[@for-label[polyglot unlike-assets]]

@title{Base Workflow: What Always Happens}

Both @secref{default-workflow} and @secref{functional-workflow}
add on to a basic set of rules we'll cover here.

Overall, a built-in workflow produces HTML5 documents in @racket[dist-rel]
with correct links and references to other content. The journey to this
state involves use of Racket modules mixed with Markdown.

@section[#:tag "rackdown"]{Mixed-mode Racket}

The workflows shipped with @racket[polyglot] target HTML @tt{<script>}
elements within Markdown files. There are @defterm{library ("lib") elements}
and @defterm{application ("app") elements}. These elements are processed in that
order, but we will cover application elements in depth first.

Each app or lib element holds the source code for a Racket module as CDATA,
including the @tt{#lang} line. The @tt{src} attribute on these @tt{<script>}
elements are ignored, because they contribute complexity without added benefit in
the context of @racketmodname[polyglot]. The @tt{src} attributes on all other script
elements work according to a given workflow.

@tt{<script id="foo" ...>} causes @racketmodname[polyglot] to write the CDATA
of that script element to @tt{<tmp>/foo.rkt}, where @tt{<tmp>} is a temporary
directory created inside @racket[(system-temp-rel)]. If you do not specify a
value for an @tt{id} attribute, @racketmodname[polyglot] will generate one for
you.

@margin-note{On Windows, your account might not have permission to create
links. Either run @racket[polyglot] as an Administrator, or (better yet) get
permission to create links.}
@margin-note{Be careful to include any Racket-level dependencies in @tt{info.rkt} or a
setup script.}
Since the Racket modules exist in the same
directory when processed together, they can @racket[require] each
other. @racketmodname[polyglot] also leaves a symlink named @tt{project}
pointing to your project directory so that multiple pages can share code and
data.


@subsection{Relationship to Markdown}
Markdown supports use of HTML tags, so we can just write the @tt{<script>} elements
we want directly among prose. This gives a nice experience where you can "drop into"
any DSL to work on the underlying page model when plain language won't cut it.
Examples in this documentation will use Markdown.

Clearly out of a fit of inspiration and unrestrained genius, we'll call the
mix of Racket and Markdown @defterm{Rackdown}. To be clear, Rackdown isn't special
and does not presume on a particular workflow. Any Markdown parser can parse pages
written for @racketmodname[polyglot].

@subsection{Application Elements}
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
  (tx-replace-me page
                 (λ (x) `((p ,(format "Today is ~a" (date->string (current-date))))))))
</script>
}|


@subsection{Library Elements}
Library elements have media type @racket["text/racket"]. Library elements are saved
to disk before application elements run. This lets you share code and data within
the page. Unlike app elements, library elements can provide any identifiers they
wish just like any other Racket module.

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

It's especially important to name your libraries with an @racket[id] attribute, or else
you will not know what name it has on disk in an application element.

@subsection{Accessing Shared Content}
Library elements provide shared content @italic{within} a page.
To share code and data @italic{across} pages, place related files
in your project directory and use the @tt{project} symlink
to access them.

@verbatim[#:indent 2]|{
<script type="text/racket" id="main">
#lang racket/base
(require "project/components.rkt")
(provide (rename-out [two-column layout]))
</script>
}|

@section{Dependency Discovery and Processing}

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

@section{Gotcha: XML in HTML5}
@racketmodname[polyglot]'s Markdown parser happens to include
XML elements embedded in your page for processing. As you learn
more about your workflow, you'll find you can generate content in terms of
those elements. This lets you write things like this:

@verbatim[#:indent 2]|{
<page title="My Page Title"
      layout="two-column"
      keywords="pizza, skateboards, cowabunga">
}|

But if you build this, it will not appear as input because you
forgot to close the elements, XHTML style. If you add the
missing @tt{/}s, it will work. You may safely omit the @tt{/}
for HTML5 void elements, however.

Since HTML5 is @italic{not} based on XML, you need to take
care to transform any XML-based content you introduce in your
pages back to valid HTML5 in the pre-built workflows.
