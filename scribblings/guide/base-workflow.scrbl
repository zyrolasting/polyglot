#lang scribble/manual

@require[@for-label[racket polyglot unlike-assets]
         "../macros.rkt"]
@title[#:tag "base-workflow"]{The Base Workflow}

As mentioned, Polyglot actually doesn't know how to build a website.
The Base Workflow provides common logic and sensible defaults for building
websites. @secref{functional-workflow} and @secref{imperative-workflow}
fill gaps in Markdown processing by extending this workflow.

To understand why the Base Workflow is important, consider what happens to a
parsed Markdown file once there are no more app or lib elements to process.  If
you convert it to HTML, you might get links to resources like
@litchar{blah.html}. How does the HTML document get that name, and how do we
know the link isn't broken?

Once app and lib elements have been processed to produce a finished page, the
Base Workflow starts a @defterm{dependency discovery} phase where it searches
for paths to other files in @tt{src} and @tt{href} attributes of the HTML
elements derived from your Markdown, and in the @tt{url()} expressions of your
CSS. The Base Workflow notices the links that are @bold{either absolute paths
on your filesystem, or paths relative to your assets directory}.

Consider this Markdown:

@verbatim[#:indent 2]|{
* [Contact me](contact.md)
* [Search the Web](https://duckduckgo.com)
* ![Generated Image](generate-image.rkt)
}|

During the dependency discovery phase, the Base Workflow will only pick up on
@litchar{contact.md} and @litchar{generate-image.rkt} because they are relative
paths to assets that are not user-facing.

Each asset referenced is processed according to the rules of their associated
workflow, but the Base Workflow offers some reasonable defaults. Each round of
processing kicks off another dependency discovery phase, and the whole cycle
will repeat until all referenced assets are crawled. The result is a working,
linked collection of HTML5 documents, complete with production-ready assets.

Let's cover the common and default asset processing rules provided by
the Base Workflow.

@section{Markdown Handling (@racket[".md"])}
The Base Workflow does not handle all Markdown processing on its own, just the
hard parts. It analyzes the final version of content produced by either
@secref{functional-workflow} or @secref{imperative-workflow}, delegates work to
resolve all dependencies, rewrites links so that they correctly point to
user-facing resources in the distribution, and writes the result as an HTML5
document.

How exactly app and lib elements modify your content before all of this
happens depends on the workflow you use.

@section[#:tag "handle-rkt"]{Racket Module Handling (@racket[".rkt"])}
You may depend on a Racket module to take full responsibility for
generating an asset.

@bold{This section requires knowledge of @racketmodname[unlike-assets]},
so you don't have to read it now. Come back when you need to handle
an exceptional case for a single asset referenced by your page,
and you don't want to modify your workflow.

The Racket module must produce a path to a file in a distribution to represent
the @italic{fulfilled} asset. The Base Workflow will trust that path is correct
and useful in a finished website.

For example, say you want to create a stylesheet using Racket. You can write
Racket code to generate optimized styles and list the module that does
it as a dependency.

To do this, write an element to depend on a Racket module.

@verbatim[#:indent 2]{
<link rel="stylesheet" href="compute-stylesheet.rkt" />
}

The module generates a simple style sheet with a content-hashed name for
cache-busting.

@racketmod[
#:file "compute-stylesheet.rkt"
racket

(provide write-dist-file)
(require file/sha1 polyglot)

(define (write-dist-file clear compiler)
  (define css "body{font-size:20px}\n")
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

To integrate with the Base Workflow, the module must @racket[provide]
@racket[write-dist-file] as an @racket[advance/c] procedure. Notice that it
returns a complete path in a distribution using @racket[dist-rel]. The
procedure writes the stylesheet in the distribution as a side-effect.

In terms of @racketmodname[unlike-assets], the path to the stylesheet
is the fulfilled @racket[unlike-asset/c]. The base workflow will accept
this path and use it to replace the original reference to the module.

The resulting HTML will look like this:

@verbatim[#:indent 2]|{
<link rel="stylesheet" href="5f9bb103.css" />
}|

The difference between this approach and writing equivalent Racket
code in an application element is @italic{when} the code runs. This
code runs after finding dependencies in a page, but before writing
HTML5 documents to disk.

@section{CSS Handling (@racket[".css"])}
In CSS files, the values in @tt{url()} expressions are treated just like the
@tt{href} and @tt{src} attribute values in HTML elements parsed from Markdown.
The Base Workflow does @italic{not} inspect the @tt{url()} expressions inside
@tt{<style>} elements.

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

Notice that this stylesheet is @italic{not} minified, and this process does not
alter or interpret @litchar{@"@"import} at-rules.

@section{Literal Path Handling (@racket[".literal"])}

Any dependency paths with extension @tt{.literal} are placed as-is in the
distribution, ironically without the @tt{.literal} extension.

This means that @litchar{[Link](doc.html.literal)} becomes @litchar{<a
href="doc.html">Link</a>}.

If you need @tt{.literal} to appear in the output, use the extension twice
(e.g. @tt{doc.literal.literal}).

@section{Default File Handling}
Any other files you reference are copied to the distribution such that the file
name is the first eight characters of the SHA1 hash of the file content. This
is useful for cache busting.
