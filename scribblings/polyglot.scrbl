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
@item{A Markdown+Racket file can have its own dependencies and therefore
won't build on every system where @racket[polyglot] is installed. Be careful
to use @racket[info.rkt] or a setup script to make your website's dependencies
available.}]

@section{Dependency discovery and processing}

You may have noticed the links in the above example go to other Markdown files.
This is a good time to bring up how @racket[polyglot] views the relationship
between your web assets.

@racket[polyglot] scans all @racket[href] and @racket[src] attribute values
in a page once it has fully expanded. If those values look like they
are meant to be paths (including @racket["file:"] URLs), they
are considered dependencies of your page.

Note that these values that are @bold{either absolute paths on
your filesystem, or paths relative to your assets directory.}
So in the above example, @racket["about.md"] corresponds to
a complete path like @racket[/home/sage/dir-from-command-line/assets/about.md].

@racket[polyglot] will iteratively discover and process any referenced Markdown
files. All Markdown files will appear in the dist directory with the same
name, except for the extension being changed to @racket[".html"].

Any non-Markdown files you reference are copied to the dist directory,
such that the file name is the first eight characters of the SHA1 hash of the
file content. This is for cache busting in general.

To customize this behavior, see @secref["extending"]

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
