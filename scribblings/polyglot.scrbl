#lang scribble/manual
@require[@for-label[aws/keys
                    polyglot
                    txexpr
                    unlike-assets
                    racket/base
                    racket/class]]

@title{Build static websites using any language}
@author{Sage Gerard}

@defmodule[polyglot]

This module uses @racket[unlike-compiler%] to generate
static websites written in Markdown and any language
supported by Racket.

The author's website uses @racket[polyglot] and @hyperlink["https://sagegerard.com/racket-powered.html"]{has a page demonstrating use}.
The @hyperlink["https://github.com/zyrolasting/polyglot/blob/master/README.md" "README in the source code"] also doubles an an example page.
The demo command builds the README using @racket[polyglot].

@verbatim[#:indent 2]|{
$ raco polyglot demo
}|

Use the demo command to verify if @racket[polyglot] works on your system.
If it works, you will see a @racket["dist"] folder appear in your
working directory containing a @racket["README.html"] file. This
directory contains a @italic{distribution} built from assets.

@section{How to prepare a @racket[polyglot] project}

@verbatim[#:indent 2]|{
$ raco polyglot start my-website
$ raco polyglot build my-website
$ cd my-website
$ raco polyglot build .
}|

The @racket["my-website"] used in the start command creates a new @italic{project directory}
with example content that uses @racket[polyglot]'s features. The code inside will contain
helpful comments.

@racket[polyglot] commands expect you to specify a path to a project directory to read, whereas @racket[start]
will try to create a directory with the given name.

When you run the @racket[build] command, @racket[polyglot] will read @racket["<project>/assets/index.md"]
and write content to the @racket["<project>/dist"] directory, creating it if it does not already exist.

As with the README, once you build the website you will see a dist directory appear.
But instead of the directory appearing in your working directory, it will always appear
in the project directory.

In this example, @racket[polyglot] only parses Markdown, wraps the content in an HTML5
document structure, and writes the resulting HTML to @racket["dist/index.html"]. You
will also see other files because @racket[polyglot] discovers dependencies of your pages
and processes them too.

@section{Writing Racket in Markdown}

@margin-note{Using script elements means that any Markdown parser can parse pages written for @racket[polyglot].
@racket[polyglot] only interprets particular elements within Markdown and does not have any parsing rules of its own.}
Markdown supports HTML tags inside its content. @racket[polyglot]
will look for script elements of media type @racket["text/racket"]
and @racket["application/rackdown"], in that order. These elements
are henceforth named @italic{library} and @italic{application} elements,
respectively.

@verbatim[#:indent 2]|{
# My page

Lorem ipsum...

<script type="text/racket" id="components">
#lang racket/base

(provide title link)

(define title "My page")
(define (link label to) `(a ((href ,to)) ,label))
</script>

<script type="application/rackdown" id="main">
#lang racket/base

(provide layout)
(require "components.rkt")

(define (layout kids)
  `(html (head (title ,title))
         (body . ,kids)))

(write (link "About" "about.md"))
(write (link "Contact" "contact.md"))
</script>

Thanks for visiting, blah blah
}|

@racket[polyglot] writes each script element of only the listed types
to a temporary directory. The name of the file will be @italic{id.rkt},
where @italic{id} matches the @racket["id"] attribute of the script element.
If one is not provided, a unique value will be chosen.

In this example @racket[polyglot] will consume the @racket["text/racket"] 
script (removing it from the output HTML) and write it to @racket["<tmp>/components.rkt"].
@racket["<tmp>/main.rkt"] will follow, and then execute.

The application script uses @secref["printing" #:doc '(lib "scribblings/reference/reference.scrbl")] in @racket[write] mode
to emit Tagged X-Expressions. The sequence of elements are
collected and will replace the script element in the internal
@racket[txexpr] representing the page.

Additionally, the Racket module from an application element
can set a layout for the page using @racket[(provide layout)].

@racket[polyglot] will continue processing a page until no
application elements remain. You can leverage this to generate
application elements within an application element. One use case
is creating code samples paired with actual output embedded in
the page.

@racketblock[
(define code '("#lang racket"
               "(write `(h1 \"Hello, meta.\"))"))

(write `(pre . ,code))
(write `(script ((id "example") (type "application/rackdown")) ,@code))
]

@subsection{Design implications}

@itemlist[
@item{If an application element behaves in a way that generates infinitely
many application elements, then a @racket[polyglot] build will not
terminate.}
@item{A Racket module created from a library element will
not be visited or instantiated until a Racket module
in an application element tries to use it. To avoid confusion
or unwanted output, avoid using the printer in the top-level
of library element code.}
@item{The dependency discovery pass will not capture @racket[src] attributes for
@racket["application/rackdown"] or @racket["text/racket"] @racket[script] elements
because those elements would be replaced by the time @racket[polyglot] starts looking
for dependencies.}
@item{A Markdown+Racket file can have its own dependencies on Racket modules and therefore
won't build on every system where @racket[polyglot] is installed. Be careful
to use @racket[info.rkt] or a setup script to make your website's dependencies
available.}]

@section{Dependency discovery and processing}

You may have noticed in an earlier example that links can go to other Markdown files.

@racketblock[
(write `(a ((href "about.md"))))
]

@racket[polyglot] scans all @racket[href] and @racket[src] attribute values
in a page once that page no longer has any application or library elements.

If any of those values look like they are meant to be paths
(including @racket["file:"] URLs), they are considered dependencies of your page.

Note that these values that are @bold{either absolute paths on
your filesystem, or paths relative to your assets directory} (See @racket[assets-rel]).
So in the above example, @racket["about.md"] corresponds to
a complete path like @racket[/home/sage/dir-from-command-line/assets/about.md].

@subsection{Markdown Handling}

@racket[polyglot] will process any referenced Markdown files just like the
one it processed before. All Markdown files will have their application
and library elements expand into tagged X-expressions as normal, and
all of @italic{their} dependencies process in turn.

@racket[polyglot] writes the resulting content as HTML5
to a file in the dist directory with the same name as the original
Markdown file, except with an @racket[".html"] extension.

@subsection{Racket Module Handling}

Any referenced @racket[".rkt"] files load via @racket[(dynamic-require path 'write-dist-file)].
The module must @racket[provide] @racket[write-dist-file] as an
@racket[advance/c] procedure. That procedure must write to some file in the
dist directory and return a complete path to the new file. @racket[polyglot]
will replace the value of the @racket[src] or @racket[href] attribute with
a path relative to the distribution for you.

This allows you to turn this...

@verbatim[#:indent 2]{
<link href="compute-stylesheet.rkt" />
}

...into this...

@verbatim[#:indent 2]{
<link href="5f9bb103.css" />
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
      (substring (sha1 port) 0 8)
      #".css"))

  (define path (dist-rel file-name))
  (close-input-port port)
  (display-to-file #:mode 'text #:exists 'replace
                   css path)
  path)
]

The difference between this approach and writing equivalent Racket
code in an application element is @italic{when} the code runs. This
code runs after the dependency discovery phase for a Markdown file,
but before writing HTML5 to disk.

You can also use this approach to programmatically @method[unlike-compiler% add!]
dependencies to the build.

@subsection{Default File Handling}

Any other files you reference are copied to the dist directory,
such that the file name is the first eight characters of the SHA1 hash of the
file content. This is strictly for cache busting.

To customize any of the above behavior, see @secref["extending"]

@section{Accessing shared content}

The temporary directory @racket[polyglot] uses to store modules will contain
a symlink to the directory you specify as your project directory.
It will always be named @racket["project"]. Use this link to
access resources that are useful for multiple pages.

@verbatim[#:indent 2]|{
<script type="application/rackdown" id="main">
#lang racket/base
(require "project/layouts.rkt")
(provide (rename-out [two-column layout]))
</script>
}|

If you are using Windows, @racket[polyglot] will likely not have permission
to create links on your system. In that case you can try running
@racket[polyglot] commands as an Administrator, or granting permission
to create links specifically.

@section{Responding to change}

Use the develop command to rebuild your website in response to changes in assets.

@verbatim[#:indent 2]|{
$ raco polyglot develop .
}|

If a Markdown file changes, dependent markdown files will not rebuild.
If a CSS file changes, dependent markdown files will only update outdated references to fulfilled dependencies.

@section[#:tag "extending"]{Extending @racket[polyglot]}

@racket[polyglot] offers sufficient flexibility for authoring content within a page,
but it handles web asset dependencies in an opinionated way.

@defclass[polyglot% unlike-compiler% ()]{
@racket[polyglot]'s compiler class. If you wish to extend your website
to do things like bundle JavaScript, just subclass @racket[polyglot%] and follow
the documentation for @racket[unlike-compiler%].

In the terminology of unlike-assets, @racket[polyglot%] uses complete paths as @racket[clear/c] names
and its @method[unlike-compiler% clarify] method will map any values of @italic{href} or @italic{src}
attributes to complete paths if they are readable on your system. This is likely enough for most cases,
so you'd probably just want to override @method[unlike-compiler% delegate] to recognize new dependencies.

You can even override Markdown processing entirely, but I wouldn't recommend it.
}

@section{Project paths}

@racket[polyglot] uses several computed paths. You'll need them to process project files.

@defthing[polyglot-project-directory (parameter/c complete-path?) #:value (current-directory)]{
This is the primary directory where @racket[polyglot] will do its work. It may
change according to user preferences and will impact the output of all below
procedures:
}

@deftogether[(
@defthing[path-el/c (and/c (or/c path-for-some-system? path-string?)
                           (not/c complete-path?))]
@defproc[(project-rel [path-element path-el/c] ...) path?]
@defproc[(assets-rel [path-element path-el/c] ...) path?]
@defproc[(dist-rel [path-element path-el/c] ...) path?]
@defproc[(polyglot-rel [path-element path-el/c] ...) path?]
@defproc[(system-temp-rel [path-element path-el/c] ...) path?]
)]{
These procedures behave like @racket[build-path], except each returns a path
relative to a different directory:

@itemlist[
@item{@racket[project-rel] is relative to @racket[(polyglot-project-directory)].}
@item{@racket[assets-rel] is relative to @racket[(project-rel "assets")]}
@item{@racket[dist-rel] is relative to @racket[(project-rel "dist")]}
@item{@racket[polyglot-rel] is relative to the @racket[polyglot] package's installation directory on your system.}
@item{@racket[system-temp-rel] is relative to the temp directory where ephemeral modules will appear when processing Racket in your pages.}
]
}

@section{Publishing to S3}

Use the publish command to upload your website to AWS S3. @racket[polyglot]'s implementation
assumes you have set up a credentials file and will use @racket[read-keys/aws-cli]
to load them.

@verbatim[#:indent 2]|{
$ raco polyglot publish . my-bucket us-east-2
}|

@subsection{Assumptions}

@itemlist[
@item{The bucket is configured for static web hosting.}
@item{The bucket will contain only the files from the dist directory.}
@item{HTML files should never be cached.}
@item{All other files should be cached forever.}
]

If you do not agree with all of that, then use the AWS CLI or the @racket[aws/s3] library
to upload your dist directory.

@subsection{Publication steps}

@itemlist[#:style 'ordered
@item{Read all keys in the bucket and compare them to the local contents of the dist directory. Remember the elements that are remote but not local.}
@item{Uploads the contents of the dist directory to the root of the bucket, overwriting any objects that already exist.}
@item{If @racket[--delete-diff] is set, delete from the bucket the objects marked in Step 1.}]

Why delete anything? Because if you want to save space, you'll notice that
@racket[polyglot] will not emit any file unless it was marked as a dependency. If
S3 holds a file that @racket[polyglot] did not emit, it's either an old
version of a file or it was never referenced as a dependency.
Your own pages won't have broken links internally, but changing the name
of a Markdown file or removing an existing HTML file will break external links
unless you have a system set up to issue HTTP 301 codes. If you want to ensure no broken
links, then do not ever use @racket[--delete-diff].

@section{License and contributions}

@racket[polyglot] uses the MIT license. @hyperlink["https://github.com/zyrolasting/polyglot/blob/master/README.md" "Here's the source code."]

I always welcome contributions and feedback. If for any reason there is a problem with the name
or license, reach out to me and the matter will be resolved immediately.
