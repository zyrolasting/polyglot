#lang scribble/manual
@require[@for-label[aws/keys
                    polyglot
                    racket/base]]

@title[#:tag "setup"]{Setup and Usage}

Polyglot requires symbolic links to function (See
@racket[make-file-or-directory-link]). To confirm that everything
works, install the package and run the @tt{demo} command.

@margin-note{You can also use @tt{raco polyglot}. It works the same way and is available for backwards compatibility.}
@verbatim[#:indent 2]|{
$ raco pkg install polyglot
$ polyglot demo
}|

If the command succeeds, then you will see a @tt{dist} folder appear
in your working directory containing a @tt{README.html} file. This is
a @italic{distribution} built from the
@hyperlink["https://github.com/zyrolasting/polyglot/blob/master/README.md"
"README in the source code"].

@section{Pick A Workflow}
On its own, Polyglot has no idea what to do with any file you
give it. It needs a @defterm{workflow} to define how to process
content.

Polyglot offers two built-in workflows that extend @secref{base-workflow}.

@itemlist[
@item{@secref{default-workflow}: Offers a PHP-like development experience where
you can "drop into" a new language and echo content into the page. Convenient and
intuitive, but full of corner-cases in big projects.}

@item{@secref{functional-workflow}: Offers a way to express content as
a composition of pure procedures. Powerful and scalable, but requires
more initial effort.}
]

Don't dwell too much on the choice. It won't be difficult to change
later. Pick a workflow that speaks to your soul, and create a new
project using one of these commands:

@verbatim[#:indent 2]|{
$ polyglot start my-website # For imperative workflow
$ polyglot start -f my-website # For functional workflow
}|

This creates a new @defterm{project directory} called @tt{my-website}
with example content, namely an @tt{assets} directory with an @tt{index.md}
Markdown file acting as the entry point for your website.

You can build the content right away.

@verbatim[#:indent 2]|{
$ polyglot build my-website
}|

The @tt{build} command reads @tt{/path/to/my-website/assets/index.md} and
writes content to the @tt{/path/to/my-website/dist} directory.

You can specify @tt{index.md} for the same effect. If you want to
work on a different part of your site, feel free to pass it the
relevant file later.

@verbatim[#:indent 2]|{
$ polyglot build my-website/assets/index.md
}|

@section[#:tag "live"]{Live Builds}
The @tt{develop} command rebuilds your website in response to changes in assets.
Let it run in another terminal while you work.

@verbatim[#:indent 2]|{
$ polyglot develop my-website
}|

@section{Tailoring your Workflow}
On creation, your website will contain a @tt{.polyglotrc.rkt} module.

@racketmod[#:file ".polyglotrc.rkt"
racket/base
(require polyglot)
(provide (rename-out [polyglot/imperative% polyglot+%]))
]

This configuration module provides @tt{polyglot+%}, a class that implements
your chosen workflow.

@itemlist[
@item{@racket[polyglot/imperative%] implements @secref{default-workflow}.}
@item{@racket[polyglot/functional%] implements @secref{functional-workflow}.}]

When you run commands targeting a project, Polyglot will look
for @tt{.polyglotrc.rkt} and use the provided class to process content. If you
do not have a config file at all, Polyglot will try processing
your content using @secref{default-workflow}.

If you want to provide your own workflow, you can always @racket[provide] a
subclass of @racket[polyglot/functional%] or @racket[polyglot/imperative%].

If you want to specify a different config file, use the @tt{-b} or @racket[--by-module]
option at the command line before your chosen subcommand:

@verbatim[#:indent 2]|{
$ polyglot -b /etc/polyglot.d/team-config.rkt build my-website
}|


@section{Publish to S3}
@tt{polyglot publish} can push content to S3.

@verbatim[#:indent 2]|{
$ polyglot publish my-website my-bucket us-east-2
}|

Before you use it, @bold{read this entire section.} If you do not agree
with ANY of it, then use the AWS CLI or the @racketmodname[aws/s3] library to
publish your website.


@subsection{Assumptions}
@itemlist[
@item{The bucket is configured for static web hosting.}
@item{HTML files should never be cached.}
@item{All other files should be cached forever.}
]


@subsection{Process}
@itemlist[#:style 'ordered
@item{Authenticate against AWS with @racket[read-keys/aws-cli].}
@item{Read all keys in the bucket and compare them to the local contents of the dist directory. Remember the elements that are remote but not local.}
@item{Uploads the contents of the dist directory to the root of the bucket, overwriting any objects that already exist.}
@item{If @racket[--delete-diff] is set on the command, delete from the bucket the objects marked in Step 2.}]

@bold{If you want to ensure no broken links, then do not ever use
@racket[--delete-diff]. You'll only want to use that option if the
space savings and hygiene are actually worth it, and if
@italic{everything} in the bucket that you need for your website is
produced by a Polyglot build.
}