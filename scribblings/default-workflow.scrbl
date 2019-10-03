#lang scribble/manual
@require[@for-label[polyglot
                    txexpr
                    unlike-assets
		    racket/rerequire
                    racket/base
                    racket/dict
                    racket/class]]

@title[#:tag "default-workflow"]{The Imperative Workflow (Default)}
@defmodule[polyglot/imperative]

The imperative workflow has a PHP-like development experience where
you can "drop into" an app element and use any @tt{#lang} to write
content to appear in place. The workflow accepts mixed-mode Markdown
as input and writes HTML5 pages to @racket[dist-rel] as output.

This approach has the benefit of being familiar and intuitive to many
web developers. This approach also works naturally with @tt{#lang}s
that print data as a side-effect. The drawback is that this workflow
does not scale well, and requires a larger API to handle more corner
cases. If you want a leaner workflow with less room for error,
use @secref{functional-workflow}.

This is the default workflow, meaning that it applies when no
workflow is specified at the @secref{cli}.

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
@racket[polyglot] would @racket[read] and place in your page.}
@item{A Racket module created from a library element will not be visited or instantiated
until an application element uses it. To avoid confusion or unwanted output,
either avoid using the printer in the top-level of library element code or capture
any output produced as a side-effect of instantiating a library element's module.}]

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

@section[#:tag "imperative-dependencies"]{Dependency Discovery and Processing}

This workflow offers special processing for Markdown files
and Racket module files. It also includes fallback processing
for any other file.

@subsection{Markdown Handling (@racket[".md"])}
You may link to other Markdown files in your pages.

@verbatim[#:indent 2]|{
* [Contact me](contact.md)
...
<nav><a href="about.md">About</a></nav>
}|

The imperative workflow will process any referenced Markdown files
to produce a working, linked collection of @racket[".html"] files.

@subsection[#:tag "handle-rkt"]{Racket Module Handling (@racket[".rkt"])}
Assume you want to create a stylesheet using Racket.
You can write Racket code to generate optimized styles and
list the module that does it as a dependency.

This allows you to turn this...

@verbatim[#:indent 2]{
<link rel="stylesheet" href="compute-stylesheet.rkt" />
}

...into this...

@verbatim[#:indent 2]{
<link rel="stylesheet" href="5f9bb103.css" />
}

...using this:

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
First, it must @racket[provide] @racket[write-dist-file] as an
@racket[advance/c] procedure that eventually returns a @racket[complete-path?]
to the processed file as a fulfilled @racket[unlike-asset/c].
That file must exist in @racket[(dist-rel)]. @racket[polyglot] will
replace the value of the @racket[src] or @racket[href] attribute based on the path you return.

The difference between this approach and writing equivalent Racket
code in an application element is @italic{when} the code runs. This
code runs after finding dependencies in a page, but before writing
HTML to disk.

@subsection{Default File Handling}
Any other files you reference are copied to @racket[(dist-rel)],
such that the file name is the first eight characters of the SHA1 hash of the
file content for cache busting.

@include-section["macros.scrbl"]

@section{Imperative Workflow API Reference}
@defclass[polyglot/imperative% unlike-compiler% ()]{
Implements the workflow documented on this page.

In the terminology of unlike-assets, @racket[polyglot/imperative%] uses complete paths as @racket[clear/c] names.

@defmethod[(clarify [unclear unclear/c]) clear/c]{
If the string looks like a readable path on your system, returns a complete path.

Relative paths are completed using @racket[(assets-rel)]. Complete paths are used as-is.

If the completed path path does not refer to a readable file, this will raise @racket[exn:fail].
}

@defmethod[(delegate [clear clear/c]) unlike-asset/c]{
This implements the default workflow. @secref{imperative-dependencies}
describes how this works in tutorial form.

If you override this, take care to call it from your subclass when you
encounter Markdown or Racket files unless you know the consequences.
}

@defmethod[(preprocess-txexprs [tx-expressions (listof txexpr?)]) (listof txexpr?)]{
In the default workflow, this method transforms @racket[tx-expressions] parsed
from a source Markdown file into a new list of tagged X-expressions. This transformation
occurs before processing any script elements with @racket[run-txexpr!].

Use this to sanitize untrusted code, generate application elements based on content,
or attach common metadata to documents.
}
}

@defthing[polyglot% class?]{
An alias for @racket[polyglot/imperative%] kept for backwards compatibility.
}

@defproc[(run-txexpr! [tx-expressions (or/c txexpr? (non-empty-listof txexpr?))]
                      [initial-layout (-> (listof txexpr?) (or/c txexpr? (listof txexpr?)))
                                      identity])
                      (or/c (listof txexpr?) txexpr?)]{
Runs @racket[tx-expressions] as if they were a collection of interdependent Racket modules.
Use this procedure to leverage the benefits of the imperative workflow without needing
the CLI or @racket[polyglot/imperative%].

Assumes that @racket[tx-expressions] contains representations of application
and library elements according to @secref{imperative-apps}.

Note that you do not have to use Markdown before using this function, or even
tagged expressions that represent valid HTML. You only need to represent
application or library @tt{<script>} elements as tagged X-expressions.

This will functionally replace Racket application and library elements inside the
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

