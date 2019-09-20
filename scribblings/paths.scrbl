#lang scribble/manual
@require[@for-label[polyglot
                    racket/base
		    racket/contract
                    racket/path]]

@title{Where Things Are}

As implied in @secref{setup}, @racket[polyglot] uses several paths to work.
You'll need them to process project files and understand how files
move through the system.

First and most importantly, there's the...

@defthing[polyglot-project-directory (parameter/c (and/c complete-path? directory-exists?)) #:value (current-directory)]{
This is where @racket[polyglot] will do its most important work.

As shown in @secref{setup}, you can specify this using the command-line interface.
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
@item{@racket[assets-rel] is relative to @racket[(project-rel "assets")]. This is where @racket[polyglot] will look for files you use to develop your website.}
@item{@racket[dist-rel] is relative to @racket[(project-rel "dist")]. This is where @racket[polyglot] will write output files that can be published for end-users to consume.}
@item{@racket[polyglot-rel] is relative to the @racket[polyglot] package's installation directory on your system. This should not be used for production, but you can use this to access private modules and experiment with self-hosting builds.}
@item{@racket[system-temp-rel] is relative to @racket[(find-system-path 'temp-dir)]. @racket[polyglot] uses this to prepare temporary files according to @secref{rackdown}}
]
}