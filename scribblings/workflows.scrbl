#lang scribble/manual
@require[@for-label[polyglot unlike-assets racket/base racket/class]]

@title[#:tag "base-workflow"]{The Base Workflow}
@defmodule[polyglot/base]

All built-in workflows accept Racket and Markdown as input and produce
HTML5 documents in a distribution directory as output. This page
covers behaviors that apply by default, and common tasks specialized
by both @secref{default-workflow} and @secref{functional-workflow}.

@include-section["paths.scrbl"]

@section[#:tag "rackdown"]{Mixed-mode Racket}

All workflows shipped with @racketmodname[polyglot] target HTML @tt{<script>}
elements within Markdown files. There are @defterm{library ("lib") elements}
and @defterm{application ("app") elements}. These elements are processed in that
order, but we will cover application elements in depth first.

Each app or lib element holds the source code for a Racket module as CDATA,
including the @tt{#lang} line. The @tt{src} attribute on these @tt{<script>}
elements are ignored, because they contribute complexity without added benefit in
the context of @racketmodname[polyglot]. The @tt{src} attributes on all other script
elements work according to a given workflow.

@subsection{Relationship to the File-system}
@tt{<script id="foo" ...>} causes @racketmodname[polyglot] to write the CDATA
of that script element to @tt{<tmp>/foo.rkt}, where @tt{<tmp>} is a temporary
directory created inside @racket[(system-temp-rel)]. If you do not specify a
value for an @tt{id} attribute, @racketmodname[polyglot] will generate one for
you.

@margin-note{On Windows, your account might not have permission to create
links. Either run @racket[polyglot] as an Administrator, or (better yet) get
permission to create links.}
Since the Racket modules exist in the same
directory when processed together, they can @racket[require] each
other. @racketmodname[polyglot] also leaves a symlink named @tt{project}
pointing to your project directory so that multiple pages can share code and
data. Be careful to include any Racket-level dependencies in @tt{info.rkt} or a
setup script.


@subsection{Relationship to Markdown}
Markdown supports use of HTML tags, so we can just write the @tt{<script>} elements
we want directly among prose. This gives a nice experience where you can use
any DSL to work on the underlying page model when plain language won't cut it.
Examples in this documentation will use Markdown.

Clearly out of a fit of inspiration and unrestrained genius, we'll call the
mix of Racket and Markdown @defterm{Rackdown}. To be clear, Rackdown isn't special
and does not presume on a particular workflow. Any Markdown parser can parse pages
written for @racketmodname[polyglot].

@subsection{Application Elements}
@margin-note{@racket["application/rackdown"] is also accepted for backwards compatibility.}

@defterm{Application elements} are HTML script elements of media type
@racket["application/racket"]. @racket[polyglot] will evaluate app
elements in the order they are encountered. How app elements work
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

In either case, application elements can modify the @racket[txexpr]
representing the page in which you are working. The intended experience
is that you can stop using prose and start applying more surgical changes
by making an incision via an application element.


@subsection{Library Elements}
Library elements have media type @racket["text/racket"]. Library elements are saved
to disk before application elements run. This lets you share code and data within
the page. Unlike app elements, library elements can provide any identifiers they
wish just like any other Racket module.

Library elements do not operate differently across workflows.

Here's a library element that provides a custom element as a
procedure. It can sit among prose just like any other element.

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

The @tt{id} is important. When a workflow writes a library element to
disk, it will appear in the same directory as other application or
library elements. This makes it easy to @racket[require] the library.

@verbatim[#:indent 2]|{
<script type="application/racket">
#lang racket/base
(require "components.rkt")

(write (offering "Basic" "FREE"))
(write (offering "Plus" "$10/month"))
(write (offering "Pro" "$20/month"))
</script>
}|


@subsection{Accessing Shared Content}

Library elements provide shared content @italic{within} a page.
To share code and data @italic{across} pages, place related files
in your project directory and use the @tt{project} symlink
to access them.

@verbatim[#:indent 2]|{
<script type="application/racket" id="main">
#lang racket/base
(require "project/components.rkt")
(provide (rename-out [two-column layout]))
</script>
}|

@section[#:tag "discovery-proc"]{Dependency Discovery and Processing}

Once there are no more app or lib elements to process among prose,
each workflow will start a @defterm{dependency discovery} phase where
it searches for paths that look like assets on disk (including
@racket["file:"] URLs).

These paths are @bold{either absolute paths on your filesystem, or
paths relative to your assets directory} (See @racket[assets-rel]).
So if there is a link to @tt{contact.md} somewhere on your page,
a built-in workflow will try to process that file from @racket[(assets-rel "contact.md")].

The base workflow supports dependency discovery in tagged
X-expressions and CSS files. Discovery occurs after some processing in
a handful of known asset types, as detailed below. Custom workflows
must implement their own discovery phase when supporting new assets,
or when changing how existing assets are processed.

@subsection{Markdown Handling (@racket[".md"])}
When you start building a website, @tt{polyglot} will look
for an @tt{index.md} file in that website's assets by default.

All Markdown files are parsed using @racket[parse-markdown].
All app and lib elements in the resulting Tagged X-Expressions
are evaluated according to the workflow loaded from the project's
@tt{.polyglotrc.rkt}.

You may link to other assets in your pages.

@verbatim[#:indent 2]|{
* [Contact me](contact.md)
...
<img src="make-image.rkt" />
<nav><a href="about.md">About</a></nav>
}|

During the dependency discovery phase, each built-in workflow will check
all @racket[href] and @racket[src] attribute values for applicable paths.
They will be processed according to the rules of their associated
workflow.

This process will repeat for all Markdown files discovered. The result is
a working, linked collection of HTML5 documents, complete with production-ready
assets.

@subsection[#:tag "handle-rkt"]{Racket Module Handling (@racket[".rkt"])}
You may depend on a Racket module to take full responsibility for
generating an asset.

This method requires knowledge of @racketmodname[unlike-assets].
Specifically, you need to know how @racket[advance/c] procedures
work. This gives you a way to produce unique and tailored assets
without needing to write a new workflow.

The Racket module must produce a path to a file in a distribution.
The workflow will trust that path is correct and useful in a complete
website.

For example, say you want to create a stylesheet using Racket. You can write
Racket code to generate optimized styles and list the module that does
it as a dependency.

To do this, write an element to depend on a Racket module.

@verbatim[#:indent 2]{
<link rel="stylesheet" href="compute-stylesheet.rkt" />
}

The module generates a simple style sheet.

@racketmod[
#:file "compute-stylesheet.rkt"
racket

(provide write-dist-file)
(require file/sha1 polyglot)

(define (write-dist-file clear compiler)
  (define css "body { font-size: 20px }\n")
  (define port (open-input-string css))
  (define file-name
    (path-replace-extension
      (substring (sha1 port) 0 8) (code:comment "For cache busting")
      #".css"))

  (define path (dist-rel file-name))
  (close-input-port port)
  (display-to-file #:mode 'text #:exists 'replace
                   css path)
  path)
]

To integrate with the base workflow, the module must @racket[provide]
@racket[write-dist-file] as an @racket[advance/c] procedure. Notice
that it returns a complete path in @racket[(dist-rel)]. The procedure
writes the stylesheet in the distribution as a side-effect.

In terms of @racketmodname[unlike-assets], the path to the stylesheet
is a fulfilled @racket[unlike-asset/c]. The base workflow will accept
this path and use it to replace the original reference to the module.

@verbatim[#:indent 2]|{
<link rel="stylesheet" href="5f9bb103.css" />
}|

The difference between this approach and writing equivalent Racket
code in an application element is @italic{when} the code runs. This
code runs after finding dependencies in a page, but before writing
HTML5 documents to disk.

@subsection{CSS Handling (@racket[".css"])}
In CSS, the values in @litchar{url()} expressions are treated just
like the @tt{href} and @tt{src} attribute values in Markdown files.

You can leverage this along with Racket module dependencies to generate
assets for presentation.

Consider this stylesheet:

@verbatim[#:indent 2]|{
@font-face {
  font-family: 'UberFont';
  src: url('uberfont.ttf');
}

body {
  background-image: url(generate-background.rkt);
  font-family: UberFont;
}
}|

By the rules discussed, it may become:

@verbatim|{
@font-face {
  font-family: 'UberFont';
  src: url('82e2ab1f.ttf');
}

body {
  background-image: url(b7ee41cd.png);
  font-family: UberFont;
}
}|

Notice that unlike the Racket module example above that
wrote a stylesheet to disk, this CSS is @italic{not}
minified. Additionally, this process does not alter
or interpret @litchar{@"@"import} at-rules.


@subsection{Default File Handling}
Any other files you reference are copied to @racket[(dist-rel)],
such that the file name is the first eight characters of the SHA1 hash of the
file content for cache busting.


@section{Hooking Before and After Builds}

@racketmodname[polyglot] workflows are just classes that
all inherit from @racket[unlike-compiler%]. @racket[unlike-compiler%]
is a class that encapsulates processing dependent assets with custom
integrations, like Webpack.

The @method[unlike-compiler% compile!] method actually executes the
given workflow and the underlying build. This method is synchronous,
so if you want to add code to occur before or after a build, simply
override it.

Here's an example that overrides the class that implements
@secref{functional-workflow}.

@racketblock[
(class polyglot/functional%
  (define/override (compile! #:changed c #:removed r)
    (displayln "before build")
    (define output (super compile! #:changed c #:removed r))
    (displayln "after build")
    output))]

@section{Base Workflow API Reference}
@defclass[polyglot/base% unlike-compiler% ()]{
Implements the common workflow concerns on this page.

In the terminology of @racketmodname[unlike-assets], @racket[polyglot/base%] uses complete paths as @racket[clear/c] names.

@defmethod[(clarify [unclear unclear/c]) clear/c]{
If the string looks like a readable path on your system, returns a complete path.

Relative paths are completed using @racket[(assets-rel)]. Complete paths are used as-is.

If the completed path path does not refer to a readable file, this will raise @racket[exn:fail].
}

@defmethod[(delegate [clear clear/c]) unlike-asset/c]{
Implements the rules shown in @secref{discovery-proc}.
}
}

@section{Addendum: Avoid Leaving XML in HTML5}
@racketmodname[polyglot]'s Markdown parser happens to include
XML elements embedded in your page for processing. As you learn
more about your workflow, you'll find you can generate content in terms of
those elements. This lets you write things like this:

@verbatim[#:indent 2]|{
<page title="My Page Title"
      layout="two-column"
      keywords="pizza, skateboards, cowabunga">
}|

But if you build this, the @tt{page} element will not appear as input because you
forgot to close the elements, XHTML style. If you add the
missing @tt{/}s, it will work. You may safely omit the @tt{/}
for HTML5 void elements, however.

Since HTML5 is @italic{not} based on XML, you need to take
care to transform any XML-based content you introduce in your
pages back to valid HTML5 in the pre-built workflows.
