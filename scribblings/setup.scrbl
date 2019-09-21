#lang scribble/manual

@title[#:tag "setup"]{Quick Start}

First, install the package and make sure it works.

@verbatim[#:indent 2]|{
$ raco pkg install polyglot
$ raco polyglot demo
}|

You will see a @racket["dist"] folder appear in your working directory
containing a @racket["README.html"] file. This is a @italic{distribution}
built from the @hyperlink["https://github.com/zyrolasting/polyglot/blob/master/README.md" "README in the source code"].

Now let's make your site.

@verbatim[#:indent 2]|{
$ raco polyglot start my-website
}|

This creates a new @italic{project directory} called @racket["my-website"]
with example content, namely an @racket["assets"] directory with an @racket["index.md"]
file acting as the entry point for your website.

Let's build.

@verbatim[#:indent 2]|{
$ raco polyglot build my-website # Or `raco polyglot build .`,
                                 # if you are in the `my-website`
				 # directory
}|

With the exception of @racket[start], @racket[polyglot] commands expect you to
specify a path to a project directory. The @racket[build] command reads
@racket["<project>/assets/index.md"] and writes content to the
@racket["<project>/dist"] directory.

As with the demo, once you build the website you will see a dist directory appear.
But instead of the directory appearing in your working directory, it will always appear
in the project directory.
