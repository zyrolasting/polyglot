#lang scribble/manual
@require[@for-label[polyglot
                    unlike-assets
                    @only-in[markdown parse-markdown]
                    racket/base
                    racket/class]]

@title{Racket in Markdown}

All of Polyglot's built-in workflows turn a mix of Racket and Markdown into
HTML5 documents. If you don't want to use Markdown or HTML5, that's perfectly
fine. We'll cover how to switch them up later. For now, using the built-in
workflows will help you get started quickly while learning how to handle
content with Polyglot.

One of Markdown's nicer features is that you can use HTML elements directly
inside of it. Polyglot's built-in workflows take advantage of this by
supporting new types of @tt{<script>} elements that hold Racket code. There are
@defterm{library ("lib") elements} and @defterm{application ("app") elements}.
When a built-in workflow encounters these elements, it saves them to disk
in a temporary directory that lasts only long enough to prepare the page.

@section{Application Elements}
@margin-note{@racket["application/rackdown"] is also accepted for backwards compatibility.}
@defterm{Application elements} are HTML script elements of media type
@racket["application/racket"]. How app elements work depends on the
workflow you use.

Here's an example from @secref{imperative-workflow}. In this workflow, app
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

Here's an equivalent example in @secref{functional-workflow},
where an app element can replace the entire page without side-effects.

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

In either case, application elements can modify the Tagged X-Expression
representing the page in which you are working. The intended experience
is that you can stop writing at any time and make an incision into your
page with an application element.

@section{Library Elements}
Library elements have media type @racket["text/racket"]. Library elements are saved
to disk before application elements run. This lets you share code and data within
the page. Unlike app elements, library elements can provide any identifiers they
wish just like any other Racket module.

Library elements do not operate differently across workflows.

Here's a library element that provides a procedure for writing a custom HTML
element as a Tagged X-Expression.  This library can sit among prose just like
any other element.

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
}|

The @tt{id} is important. When a workflow sees a library element, it will write
it to disk next to other modules from application and library elements. This
makes it easy to @racket[require] the library.

@verbatim[#:indent 2]|{
<script type="application/racket">
#lang racket/base
(require "components.rkt")

(write (offering "Basic" "FREE"))
(write (offering "Plus" "$10/month"))
(write (offering "Pro" "$20/month"))
</script>
}|

@section{Accessing Shared Content}

Library elements provide shared content @italic{within} a page. To share code
and data @italic{across} pages, place the goods in your project directory and
use the generated @tt{project} symlink to access them.

@verbatim[#:indent 2]|{
<script type="application/racket" id="main">
#lang racket/base
(require "project/components.rkt")
(provide (rename-out [two-column layout]))
</script>
}|

@section[#:tag "xml-html5-caveat"]{Addendum: Avoid Leaving XML in HTML5}
Polyglot's Markdown parser happens to include XML elements embedded in your
page for processing. As you learn more about your workflow, you'll find you can
generate content in terms of those elements. This lets you write things like
this and have it mean something:

@verbatim[#:indent 2]|{
<page title="My Page Title"
      layout="two-column"
      keywords="pizza, skateboards, cowabunga">
}|

But if you build a project with this markup, the @tt{page} element will not
appear because you forgot to close the elements, XHTML style. If you add the
missing @tt{/} before the @tt{>}, it will work. You may safely omit the @tt{/}
for HTML5 void elements, however.

Since HTML5 is @italic{not} based on XML, you need to take special care to
transform any XML-based content you introduce for convenience back to valid
HTML5 in the built-in workflows.
