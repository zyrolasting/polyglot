#lang scribble/manual
@require[@for-label[polyglot file-watchers unlike-assets unlike-assets/logging]]

@title{@tt{polyglot} CLI}

To simplify use, Polyglot comes with a CLI that acts as a front-end to
the @racketmodname[polyglot] collection.

The @tt{build}, @tt{demo} and @tt{develop} commands forward events
from @racket[unlike-assets-logger] to STDOUT. The @tt{build} and
@tt{demo} commands report the number of warnings and errors
encountered during processing.

@section{@tt{polyglot start}: Start a Project}

@verbatim[#:indent 2]|{
$ polyglot start -f my-functional-website
$ polyglot start my-imperative-website
}|

The @tt{start} command creates a project directory with the given name
in the working directory. By default, the project will use
@racket[polyglot/imperative%] and include some supported starter
code. If you specify @litchar{-f} or @litchar{--functional}, the
project will reflect use of @racket[polyglot/functional%] instead.

@section{@tt{polyglot build}: Build a Project Once}

The @tt{build} command accepts a path as an argument and attempts to
build a project using a workflow. If a workflow cannot be determined,
the @tt{build} command will try to use @racket[polyglot/imperative%]
for backwards-compatibility reasons. If the path is a relative path,
it will be converted to a complete path relative to the current
working directory.

The behavior will vary slightly if you specify a directory or a file.

@subsection{Specifying a Directory}

@verbatim[#:indent 2]|{
$ polyglot build my-website
}|

When you specify a directory, the @tt{build} command will try to
require @racket[polyglot+%] from @tt{my-website/.polyglotrc.rkt},
falling back to @racket[polyglot/imperative%] on failure. It will then
instantiate the selected workflow class, and start processing
from @tt{my-website/assets/index.md}.

@subsection{Specifying an Asset File}

@verbatim[#:indent 2]|{
$ polyglot build my-website/assets/styles/styles.css
}|

When you specify a file, the @tt{build} command will require
@racket[polyglot+%] from the @tt{.polyglotrc.rkt} in the nearest
project directory, falling back to @racket[polyglot/imperative%] on
failure. It will then instantiate the selected workflow class, and
start processing from the asset specified in the command line.

@section{@tt{polyglot develop}: Build a Project Live}
The @tt{develop} command builds a project once, then rebuilds your
website in response to changes in assets detected using
@racket[robust-watch]. It will stop when you press Control-C.

@verbatim[#:indent 2]|{
$ polyglot develop my-website
}|

The rules for how the @tt{develop} command treats paths are the same
as the @tt{build} command.

@section{@tt{polyglot demo}: Build Demo Project}

@verbatim[#:indent 2]|{
$ polyglot demo
}|

The @tt{demo} command is a special case of @tt{build} that targets the README
from Polyglot's own source code. The distribution directory will appear in
the working directory. This command is meant to verify that a Polyglot
installation is supported on the target platform and is working as intended.

@section{@tt{polyglot publish}: Publish to S3}

The @tt{publish} command builds a project once, then writes the
contents of a project's @tech{distribution directory} to an AWS S3
bucket. The rules for how the @tt{publish} command treats paths are
the same as the @tt{build} command.

@verbatim[#:indent 2]|{
$ polyglot publish my-website my-bucket
}|

Before you use this command, @bold{read this entire section.} If you
do not agree with ANY of it, then use the AWS CLI or the
@racketmodname[aws/s3] library to publish your website.

Use the @litchar{-d} or @litchar{--dry-run} switch to
avoid writing content to S3 and instead merely report
what the command would otherwise do to the bucket.

@verbatim[#:indent 2]|{
$ polyglot publish -d my-website my-bucket
}|

Use the @litchar{-r} or @litchar{--region} switch to
change the applicable S3 region.

@verbatim[#:indent 2]|{
$ polyglot publish -r us-east-2 my-website my-bucket
}|

Use the @litchar{--delete-diff} switch to delete all objects in the S3
bucket that are not part of the distribution uploaded to the
bucket. Most people won't need this.

@verbatim[#:indent 2]|{
$ polyglot publish --delete-diff my-website my-bucket
}|

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
@racket[--delete-diff]}. You'll only want to use that option if the
space savings and hygiene are actually worth it, and if
@italic{everything} in the bucket that you need for your website is
produced by a Polyglot build.

@section{Shared Command Options}

Some command-line flags may be specified after @tt{polyglot} but
before a subcommand.

Use the @litchar{-v} or @litchar{--verbose} option to include debug
level events from @racket[unlike-assets-logger] in STDOUT.

@verbatim[#:indent 2]|{
$ polyglot -v build some-site
}|

Use the @litchar{-b} or @litchar{--by-module} option with a path
to a Racket module to forcibly use that module in place of a project's
@tt{.polyglotrc.rkt}.

@verbatim[#:indent 2]|{
$ polyglot -b /etc/polyglot.d/shared-config.rkt build some-site
}|
