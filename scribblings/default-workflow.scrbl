#lang scribble/manual
@require[@for-label[polyglot
                    txexpr
                    unlike-assets
		    racket/rerequire
                    racket/base
                    racket/dict
                    racket/class]]

@title[#:tag "default-workflow"]{Default Workflow}

@racket[polyglot] ships with a command-line tool that makes you follow a certain workflow.
We cover what it does here in more detail.

We saw @racket[polyglot] generate static websites written in Markdown and any
language supported by Racket in @racket[<script>] elements. The use of Markdown
is not required in a custom workflow, but it is expected when using the CLI.

@section[#:tag "dependencies"]{How @racket[raco polyglot build] Works}

@itemlist[
#:style 'ordered
@item{Set @racket[polyglot-project-directory] to the path provided by the user.}
@item{Parse @racket[(assets-rel "index.md")]}
@item{Process all applicable script elements according to @secref{rackdown}}
@item{Process any dependencies}
]

Let's talk about the dependency step.

Once there are no more application or library elements to process
in a Markdown file, @racket[polyglot] checks all @racket[href]
and @racket[src] attribute values in a page and keeps the ones
that look like assets on your disk (including @racket["file:"] URLs).
These values are @bold{either absolute paths on your filesystem, or
paths relative to your assets directory} (See @racket[assets-rel]).

So if there is a link to @racket["contact.md"] somewhere on
your page during Step 4, @racket[polyglot] will try to process that
file from @racket[(assets-rel "contact.md")].

For each dependency, @racket[polyglot] will do one of the below depending
on the file extension:

@subsection{Markdown Handling (@racket[".md"])}

You may link to other Markdown files in your pages.

@verbatim[#:indent 2]|{
* [Contact me](contact.md)
...
<nav><a href="about.md">About</a></nav>
}|

@racket[polyglot] will process any referenced Markdown files according to
@secref{rackdown}. This process will iteratively consume all referenced Markdown
to produce a working, linked collection of @racket[".html"] files in
the dist directory with the same name (sans extension) of the original Markdown files.

@subsection[#:tag "handle-rkt"]{Racket Module Handling (@racket[".rkt"])}

Assume you want to create a stylesheet using Racket, which
has nice things like @seclink["top" #:doc '(lib "css-expr/css-expr.scrbl") "css-expr"].
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

There's a lot happening here that you don't see. Let's bring it all
out into the open.

Any referenced @racket[".rkt"] files load via @racket[(dynamic-require
(assets-rel src-or-href) 'write-dist-file)] (or just
@racket[src-or-href], if its a complete path). If you are writing a
website according to @secref{live}, @racket[dynamic-rerequire]
captures your changes to any Racket modules in your assets directory.

The difference between this approach and writing equivalent Racket
code in an application element is @italic{when} the code runs. This
code runs after finding dependencies in a page, but before writing HTML5 to disk.

Use this approach to "spot optimize" assets and defer preparation
of certain files until after the processing stage for a page. See
@secref{extending} for other uses.

Note that the module is following a particular format that assumes
familiarity with how @racket[unlike-compiler%] works.

First, it must @racket[provide] @racket[write-dist-file] as an
@racket[advance/c] procedure that eventually returns a @racket[complete-path?]
to the processed file as a fulfilled @racket[unlike-asset/c].
That file must exist in @racket[(dist-rel)].

@racket[polyglot] will replace the value of the @racket[src] or @racket[href]
attribute based on the path you return.

@subsection{Default File Handling}

Any other files you reference are copied to @racket[(dist-rel)],
such that the file name is the first eight characters of the SHA1 hash of the
file content for cache busting.

To customize any of the above behavior, see @secref{extending}.

@section[#:tag "live"]{Live Builds}

Let's step back to the CLI. Use the @racket[develop] command to
rebuild your website in response to changes in assets.

@verbatim[#:indent 2]|{
$ raco polyglot develop my-website
}|

If a Markdown file changes, dependent markdown files will not rebuild.
If a non-Markdown file changes, dependent Markdown files will repeat Step 4
in @secref{dependencies}.

@section[#:tag "extending"]{Extending @racket[polyglot]}

@racket[polyglot] depends on @seclink["top" #:doc '(lib "unlike-assets/scribblings/unlike-assets.scrbl") "unlike-assets"] under the hood. @racket[unlike-assets] models dependencies between assets that are, well, not like one another. An HTML page depending on images, Racket files, and Markdown files are an example of that.

@subsection{Specialized @racket[unlike-compiler%] Class}

@defclass[polyglot% unlike-compiler% ()]{
@racket[polyglot]'s compiler class. If you wish to extend your website
to do things like bundle JavaScript, just subclass @racket[polyglot%] and follow
the documentation for @racket[unlike-compiler%].

In the terminology of unlike-assets, @racket[polyglot%] uses complete paths as @racket[clear/c] names.

@defmethod[(clarify [unclear unclear/c]) clear/c]{
If the string looks like a readable path on your system, returns a complete path.

Relative paths are completed using @racket[(assets-rel)]. Complete paths are used as-is.

If the completed path path does not refer to a readable file, this will raise @racket[exn:fail].
}

@defmethod[(delegate [clear clear/c]) unlike-asset/c]{
This implements the default workflow. @secref{dependencies}
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

@include-section["macros.scrbl"]
@include-section["publishing.scrbl"]