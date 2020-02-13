#lang scribble/manual

@require[@for-label[racket/base
                    racket/class
                    racket/contract
                    racket/rerequire
                    polyglot
                    unlike-assets]]

@title{@tt{polyglot/builds}}
@defmodule[polyglot/builds]

The following procedures apply @tech{workflows} to @tech{projects}.

@defproc[(make-polyglot-workflow-object [project (is-a?/c polyglot-project%)]
                                        [#:live? live? boolean? #t]
                                        [#:forced-workflow
                                           forced-workflow
                                           (or/c #f (subclass?/c unlike-compiler%)) #f]
                                        [#:fallback-workflow
                                           fallback-workflow
                                           (or/c #f (subclass?/c unlike-compiler%)) #f])
                                       (is-a?/c unlike-compiler%)]{
This is a factory function that returns a new instance of a workflow class.
During instantiation, @racket[polyglot-project-directory] equals @racket[(get-field directory project)].

The factory will use the first class available among the following expressions:

@itemlist[#:style 'ordered
@item{@racket[forced-workflow]}
@item{@racket[(send project get-workflow-class #:live? live? (lambda _ fallback-workflow))]}
]

If no class is found, the procedure will raise @racket[exn:fail].
}

@defproc[(build-polyglot-project! [project (is-a?/c polyglot-project%)]
                                  [compiler (is-a?/c unlike-compiler%)]
                                  [#:changed changed (listof clear/c) '()]
                                  [#:removed removed (listof clear/c) '()])
                                  dict?]{
Equivalent to

@racketblock[
(parameterize ([polyglot-project-directory (get-field directory project)])
  (unless (directory-exists? (dist-rel))
    (send project ensure-empty-distribution!))
  (send compiler compile! #:changed (keep changed) #:remove (keep removed)))]
}

where @racket[keep] is a function that filters out any paths that are
not asset paths in @racket[project].

@defproc[(make-polyglot-builder [project-directory useable-polyglot-directory?]
                                [#:cycle-after cycle-after (or/c #f exact-positive-integer?) 100]
                                [#:forced-workflow
                                   forced-workflow
                                   (or/c #f (subclass?/c unlike-compiler%)) #f]
                                [#:fallback-workflow
                                  fallback-workflow
                                  (or/c #f (subclass?/c unlike-compiler%)) #f]
                                [#:entry-assets entry-assets (non-empty-listof unclear/c) '("index.md")]
                                [#:gc-request gc-request (or/c 'major 'minor 'incremental) 'major])
                                (->* ()
                                     (#:changed (listof clear/c)
                                      #:removed (listof clear/c))
                                      dict?)]{
Returns a builder procedure that supports ongoing builds of the project located at
@racket[project-directory]. The procedure uses
@racket[make-polyglot-workflow-object] and @racket[build-polyglot-project!] for
you.

@racketblock[
(define project-directory (build-path (current-directory) "my-project"))
(define build! (make-polyglot-builder project-directory #:entry-assets '("index.md")))

(define (asset-paths . names)
  (map (lambda (el) (build-path project-directory "assets" el)) names))

(build!)
(build! #:changed (asset-paths "contact.md"))
(build! #:removed (asset-paths "junk.md") #:changed (asset-paths "about.md"))
(code:comment "...")
]

@margin-note{Beware that live builds may not work as expected if you load a
project's @tt{.polyglotrc.rkt} file on your own without reload support (See
@racket[dynamic-rerequire]). To be safe and avoid confusion, use only the
procedures in @racketmodname[polyglot/projects] or
@racketmodname[polyglot/builds] to handle workflows.}
@racket[forced-workflow] and @racket[fallback-workflow] work as they do
in @racket[make-polyglot-workflow-object]. @racket[#:live?] is always set to
@racket[#t] when using @racket[make-polyglot-builder].

@racket[#:changed] and @racket[#:removed] assets are @racket[clear/c]
names for parity with @racket[build-polyglot-project!]. On the other
hand, whenever the builder procedure creates a workflow object
@racket[W], it will stage each @racket[unclear/c] asset name
@racket[A] in @racket[entry-assets] for processing using @racket[(send
W add! (send W clarify A))] (See @racket[unlike-compiler%]). By a
default convention, @racket{index.md} is the @racket[unclear/c] name
of the initial page of any Polyglot project. You will not be able to
@method[unlike-compiler% add!] additional assets to the compiler
except through the workflow implementation.

If @racket[cycle-after] is a positive integer, then after every
@racket[cycle-after] builds the compiler will be replaced with a fresh
instance. @racket[(collect-garbage gc-request)] will follow. This counteracts
entropy on the underlying dependency graph during work, reducing the odds of
surprises during long work sessions at the cost of periodic rebuilds. It also
limits memory consumption for some expensive builds. To disable this feature,
set @racket[cycle-after] to @racket[#f].
}

@defthing[polyglot-live? (parameter/c boolean?) #:value #f]{
When @racket[#t], you may assume that the end user expects an ongoing
build. The @tt{polyglot develop} CLI command sets this to @racket[#t]
immediately.

If you are writing your own build service using polyglot, set this to
@racket[#t] before instantiating any workflow class. Check this value
if you are writing a workflow that behaves differently during live
builds, such as kicking off a development server.

While it is possible to set this to @racket[#f] within the dynamic
extent of a workflow that applies other workflows in an opinionated
way, be sure you understand the consequences of doing so.
}
