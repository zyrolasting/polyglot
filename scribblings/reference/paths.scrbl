#lang scribble/manual
@require[@for-label[polyglot
                    racket/base
		    racket/contract
                    racket/path]]

@title{@tt{polyglot/paths}}
@defmodule[polyglot/paths]

@defthing[polyglot-project-directory (parameter/c (and/c complete-path? directory-exists?)) #:value (current-directory)]{
This sets the base directory in which Polyglot will read asset files
and write distribution files. If you encounter incorrect file I/O
activity, make sure this value is what you expect it to be.

You normally set this using @tt{polyglot} command lines.
}

@defthing[polyglot-assets-directory (parameter/c (and/c complete-path? directory-exists?)) #:value #f]{
This is a derived parameter that controls the base directory of @racket[assets-rel].

When @racket[#f] (default), @racket[(polyglot-assets-directory)] will
evaluate to @racket[(build-path (polyglot-project-directory)
"assets")].
}

@defthing[polyglot-dist-directory (parameter/c complete-path?) #:value #f]{
This is a derived parameter that controls the base directory of @racket[dist-rel].

When @racket[#f] (default), @racket[(polyglot-dist-directory)] will
evaluate to @racket[(build-path (polyglot-project-directory) "dist")].

Unlike @racket[(polyglot-assets-directory)], a directory referenced by
@racket[(polyglot-dist-directory)] is not expected to exist.
}

@defthing[polyglot-temp-directory (parameter/c (and/c complete-path? directory-exists?)) #:value (find-system-path 'temp-rel)]{
Polyglot uses this directory to read and write files contained within
your content. To increase performance, set this to a directory using
an in-memory filesystem like @tt{tempfs} (Assuming that
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
@item{@racket[assets-rel] is relative to @racket[(polyglot-assets-directory)]. This is where Polyglot will look for files you use to develop your website.}
@item{@racket[dist-rel] is relative to @racket[(polyglot-dist-directory)]. This is where Polyglot will write output files that can be published for end-users to consume.}
@item{@racket[system-temp-rel] is relative to @racket[(polyglot-temp-directory)].}
@item{@racket[polyglot-rel] is relative to the Polyglot package's installation directory on your system. This should not be used for production, but you can use this to access private modules and experiment with self-hosting builds.}
]
}

@defproc[(make-dist-path-string [base complete-path?] [relative-to complete-path? (dist-rel)]) path-string?]{
Given two complete paths, returns a relative path string that can be
used in HTML, CSS, etc. to request an asset relative to the code's
enclosing file.

Behaves like @racket[find-relative-path], except if the simple forms
of @racket[base] and @racket[relative-to] are equal, this will return
@racket["/"] (meaning "webroot").
}
