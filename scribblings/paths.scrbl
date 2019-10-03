#lang scribble/manual
@require[@for-label[polyglot
                    racket/base
		    racket/contract
                    racket/path]]

@title{Where Things Are}
@defmodule[polyglot/paths]

As implied in @secref{setup}, @racket[polyglot] uses several paths to work.
They all come from the @racket[polyglot/paths] module. You'll need these
paths to process project files and understand how files move through the system.

First and most importantly, there's the...

@defthing[polyglot-project-directory (parameter/c (and/c complete-path? directory-exists?)) #:value (current-directory)]{
This is where @racket[polyglot] will do its most important work.

As shown in @secref{setup}, you can specify this using the command-line interface.
}

@defthing[polyglot-temp-directory (parameter/c (and/c complete-path? directory-exists?)) #:value (find-system-path 'temp-rel)]{
This is where @racket[polyglot] will store temporary Racket modules generated from your source code.

This directory will experience frequent reads and writes during
processing. To increase performance, set this to a directory using an
in-memory filesystem like @tt{tempfs} (Assuming that
@racket[(find-system-path 'temp-rel)] is not already on an in-memory
filesystem).
}

@deftogether[(
@defthing[path-el/c (and/c (or/c path-for-some-system? path-string?)
                           (not/c complete-path?))]
@defproc[(project-rel [path-element path-el/c] ...) complete-path?]
@defproc[(assets-rel [path-element path-el/c] ...) complete-path?]
@defproc[(dist-rel [path-element path-el/c] ...) complete-path?]
@defproc[(polyglot-rel [path-element path-el/c] ...) complete-path?]
@defproc[(system-temp-rel [path-element path-el/c] ...) complete-path?]
)]{
These procedures behave like @racket[build-path], except each returns a path
relative to a different directory:

@itemlist[
@item{@racket[project-rel] is relative to @racket[(polyglot-project-directory)].}
@item{@racket[assets-rel] is relative to @racket[(project-rel "assets")]. This is where @racket[polyglot] will look for files you use to develop your website.}
@item{@racket[dist-rel] is relative to @racket[(project-rel "dist")]. This is where @racket[polyglot] will write output files that can be published for end-users to consume.}
@item{@racket[polyglot-rel] is relative to the @racket[polyglot] package's installation directory on your system. This should not be used for production, but you can use this to access private modules and experiment with self-hosting builds.}
@item{@racket[system-temp-rel] is relative to @racket[(polyglot-temp-directory)]. @racket[polyglot] uses this to prepare temporary files according to @secref{rackdown}}
]
}
