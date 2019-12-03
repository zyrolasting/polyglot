#lang scribble/manual

@require[@for-label[racket
                    racket/rerequire
                    polyglot
                    unlike-assets]]

@title{@tt{polyglot/projects}}
@defmodule[polyglot/projects]

@racketmodname[polyglot/projects] defines Polyglot project structure
and common project-level operations.

A @deftech{project} is defined as a directory in a file system that
contains, at minimum, an @deftech{assets directory} named
@tt{assets}. The directory containing the assets directory is called
the @deftech{project directory}. When a project is constructed, a new
@deftech{distribution directory} named @tt{dist} will appear in the
project directory.

A project directory may contain a @tt{.polyglotrc.rkt} runtime
configuration file. If it exists, it must @racket[(provide
polyglot+%)], where @racket[polyglot+%] is a workflow class like
@racket[polyglot/functional%], @racket[polyglot/imperative%], or a
subclass of @racket[polyglot/base%] or @racket[unlike-assets%] for
lower-level workflows. If @tt{.polyglotrc.rkt} does not exist or fails
to provide a workflow class, this module does not define how to
respond.

@defclass[polyglot-project% object% (equal<%>)]{
@defconstructor[([directory useable-polyglot-directory?])]{
An instance of this class encapsulates file I/O and validation for a
Polyglot project located at @racket[directory].

Two instances are @racket[equal?] if their @racket[directory] fields are @racket[equal?].

When relevant, methods use @racket[parameterize] to set @racket[polyglot-project-directory]
to @racket[directory].
}

@defmethod[(asset-path? [complete-path complete-path?]) boolean?]{
Returns @racket[#t] if @racket[complete-path] is a valid asset
path for the project. This method does not consult the
filesystem.
}

@defmethod[(get-workflow-class [fail-thunk (-> any) (lambda () ...)] [#:live? live? any/c #f]) (or/c class? #f)]{
If @tt{.polyglotrc.rkt} does not exist in the project, this returns the result of @racket[fail-thunk].
By default, @racket[fail-thunk] raises @racket[exn:fail].

If @tt{.polyglotrc.rkt} exists in the project, this is equivalent to:

@racketblock[
(dynamic-require (build-path directory ".polyglotrc.rkt") 'polyglot+% fail-thunk)
]

If @racket[live?] is a true value, then this method will lead with a
@racket[dynamic-rerequire].
}

@defmethod[(ensure-empty-distribution!) void?]{
Deletes @racket[(dist-rel)] if it already exists, then creates an empty directory
in its place.
}

@defmethod[(get-directory-name) string?]{
Returns the name of the project directory. Equivalent to:

@racketblock[
(let-values ([(base name must-be-dir?)
              (split-path (get-field directory project))])
  name)
]

}
}

@defproc[(copy-polyglot-skeleton-project! [name (or/c "functional" "imperative")]
                                          [destination (or/c path-string? path?)
                                                       (make-temporary-file "polyglot~a" 'directory)]
                                          [#:force? force? any/c #f])
                                          (is-a?/c polyglot-project%)]{
Copies a polyglot project template by @racket[name] to @racket[destination],
and returns a project object pointing to @racket[destination]. If
@racket[force?] is a true value, @racket[destination] is deleted in advance.

The @tt{polyglot start} command uses this procedure.
}

@defproc[(useable-polyglot-directory? [directory path?]) boolean?]{
Assuming @racket[polyglot-project-directory] was set to @racket[directory],
returns @racket[#t] if all of the following are true:

@itemlist[
@item{@racket[directory] exists and is both readable and writeable.}
@item{The asset directory exists and is readable.}
@item{The @tt{.polyglotrc.rkt} does not exist, OR it does exist and is readable.}
]

This method does not attempt to load or instantiate @tt{.polyglotrc.rkt} if it exists.
}

@defproc[(find-closest-project [start directory-exists?]) (or/c (is-a?/c polyglot-project%) boolean?)]{
Returns a project object for the first directory in (@tt{start},
@tt{start/..}, @tt{...} , @tt{<root>}) where @racket[useable?] is
@racket[#t]. Returns @racket[#f] otherwise.
}
