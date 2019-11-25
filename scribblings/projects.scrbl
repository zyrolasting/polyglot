#lang scribble/manual

@require[@for-label[racket/base
                    racket/class
                    racket/contract
                    racket/rerequire
                    polyglot/projects]]

@title{Managing Projects Programatically}
@defmodule[polyglot/projects]

Use this module to verify project structure, associate paths
to projects, and manage workflows for a custom build process.

@defclass[polyglot-project% object% (equal<%>)]{
@defconstructor[([directory useable-polyglot-directory?])]{
This class encapsulates file I/O and validation for @tt{polyglot}
projects within a directory.

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

@defproc[(useable-polyglot-directory? [directory path?]) boolean?]{
Assuming @racket[polyglot-project-directory] was set to @racket[directory],
returns @racket[#t] if all of the following are true:

@itemlist[
@item{The project exists and is both readable and writeable.}
@item{The asset directory exists and is readable.}
@item{The @tt{.polyglotrc.rkt} runtime configuration file exists and is readable.}
]

This method does not attempt to load or instantiate @tt{.polyglotrc.rkt}.
}

@defproc[(find-closest-project [start directory-exists?]) (or/c (is-a?/c polyglot-project%) boolean?)]{
Returns a project object for the first directory in (@racket[start],
@racket[start/..], @tt{...} , @tt{<root>}) where @racket[useable?] is
@racket[#t]. Returns @racket[#f] otherwise.
}
