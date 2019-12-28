#lang scribble/manual

@require[@for-label[racket polyglot unlike-assets]]

@title{@tt{polyglot/base}}
@defmodule[polyglot/base]

The base workflow sets rules shared by @racket[polyglot/functional%]
and @racket[polyglot/imperative%] by specializing the behavior of
@racket[unlike-compiler%].

@defclass[polyglot/base% unlike-compiler% ()]{
Implements the base workflow. An instance of @racket[polyglot/base%]
does not set any rules for application elements.

In the terminology of @racketmodname[unlike-assets],
@racket[polyglot/base%] uses complete paths as @racket[clear/c] names.
Fulfilled assets are represented as complete paths to
files in a distribution directory.

@defmethod[(clarify [unclear unclear/c]) clear/c]{
If the string looks like a readable path on your system, returns a complete path.

Relative paths are completed using @racket[(assets-rel)]. Complete paths are used as-is.

If the completed path does not refer to a readable file, this will raise @racket[exn:fail]
unless the path extension equals @racket[#".literal"].
}

@defmethod[(delegate [clear clear/c]) unlike-asset/c]{
Selects an @racket[advance/c] procedure to act as the first
representation of an asset with the given @racket[clear] path.

The procedure depends on the result of @racket[(path-get-extension clear)]:

@itemlist[
@item{@racket[#".literal"]: The path will be treated as a fulfilled asset without further processing. The path may refer to a non-existent file in the project's distribution.}
@item{@racket[#".css"]: The path is assumed to refer to a CSS file. For every @litchar{url(X)} expression in the stylesheet, the compiler will @method[unlike-compiler add!] @litchar{X} as a dependency of the CSS file.}
@item{@racket[#".rkt"]: The path is assumed to refer to a Racket module. The module must @racket[(provide write-dist-file)], where @racket[write-dist-file] is an @racket[advance/c] procedure that may return a complete path as a fulfilled file in a distribution.}
@item{In all other cases, the file located at path @racket[clear] will be copied directly to the distribution directory, and renamed. The new name of the file will equal the first 8 characters of the SHA-1 hash of the file's own contents, with the same extension. This is for cache-busting purposes. The asset is then considered fulfilled with a path to the newly-renamed file.}
]
}
}

@defproc[(make-minimal-html-page [body (listof xexpr?)]) txexpr?]{
Returns @racket[`(html (head (title "Untitled")) (body ,@body))]
}

@defproc[(add-dependencies! [clear clear/c] [compiler (is-a?/c unlike-compiler%)] [txexpr/expanded txexpr?]) advance/c]{
Assuming @racket[clear] is a path to a Markdown file, and
@racket[txexpr/expanded] is @italic{derived} from that file in a given
workflow, this procedure will discover and @method[unlike-compiler%
add!] dependencies in @racket[txexpr/expanded] and return an
@racket[advance/c] procedure that prepares a final HTML5 document with
rewritten links to production-ready assets in a distribution.

Specifically, @racket[add-dependencies!] maps the output of
@racket[(discover-dependencies txexpr/expanded)] to clear names using
@racket[compiler] and adds them to the build using @racket[(send
compiler add!)]. As a special case, Markdown dependencies are added to
the compilation but without a dependency relationship on
@racket[clear]. This is to avoid a circular dependency locking up a
build when two pages link to each other.

The returned @racket[advance/c] procedure @italic{must} be used as the
next step for the asset named by @racket[clear]. That procedure will
resolve the dependencies in @racket[txexpr/expanded], write a finished
HTML5 document to a distribution, and fulfill the asset using the path
of the HTML5 document.

You will likely not call this yourself, but you can use it for custom
workflows derived from @racket[polyglot/base%] if you aim to create
HTML5 documents from Markdown using a different set of rules. The
dependency resolution step is tedious, and this will take care of it
for you.

For an example, see the functional workflow's
@method[polyglot/functional% delegate] implementation.
}
