#lang scribble/manual
@require[@for-label[aws/keys
                    polyglot
                    racket/base]]

@title{Publish to S3}

@racket[polyglot] can push content to S3.

@verbatim[#:indent 2]|{
$ raco polyglot publish my-website my-bucket us-east-2
}|

Before you use it, @bold{read this entire section.} If you do not agree
with ANY of it, then use the AWS CLI or the @racket[aws/s3] library to
publish the contents of @racket[(dist-rel)].

@section{Assumptions}

@itemlist[
@item{The bucket is configured for static web hosting.}
@item{The bucket will contain only what @racket[polyglot] built.}
@item{HTML files should never be cached.}
@item{All other files should be cached forever.}
]

@section{Process}

@itemlist[#:style 'ordered
@item{Authenticate against AWS with @racket[read-keys/aws-cli].}
@item{Read all keys in the bucket and compare them to the local contents of the dist directory. Remember the elements that are remote but not local.}
@item{Uploads the contents of the dist directory to the root of the bucket, overwriting any objects that already exist.}
@item{If @racket[--delete-diff] is set on the command, delete from the bucket the objects marked in Step 1.}]

Why delete anything? Because if you want to save space, you'll notice
that @racket[polyglot] will not emit any file unless it was marked as
a dependency. If S3 holds a file that @racket[polyglot] did not emit,
it's either an old version of a file or it was never a
dependency. Your own pages won't have broken links internally, but
changing the name of a Markdown file or removing an existing HTML file
will break external links unless you have a system set up to issue
HTTP 301 codes. @bold{If you want to ensure no broken links, then do not
ever use @racket[--delete-diff].}