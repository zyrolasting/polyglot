#lang scribble/manual

@require[@for-label[racket/base racket/runtime-path polyglot unlike-assets]]

@title[#:tag "multiple-workflows"]{Multiple Workflows}
Even if a workflow does some things well, you will eventually tire of
accomodating it for what it does @italic{not} do well. So what do you
do with existing code if you use a better, but incompatible workflow?

To maintain backwards compatibility, just keep the old workflow
around and reference its output where needed in a new project.
If you want to port code, you can do so at your own pace.

@section{Use @tt{.literal} to Mark External Dependencies}
You cannot directly depend on an asset used in a different workflow
because the dependencies can form cycles and prevent builds from
terminating. @secref{base-workflow} supports @tt{.literal} links to
request resources relative to the project's distribution. This frees
one workflow to fulfill resources for another without conflicting
builds.

Under the base workflow, this Markdown will produce two of the same link in HTML:

@verbatim[#:indent 2]|{
[Click](scores.html.literal)
[Click](scores.md)

...becomes

<a href="scores.html">Click</a>
<a href="scores.html">Click</a>
}|

If you remove @tt{scores.md} in the original Markdown, then the HTML
will contain just one broken link. Reintroducing the link to
@tt{scores.md} will fix the problem, but the method does not actually
matter.

@tt{.literal} links are also useful because if you are writing a list in
a prose language, interjecting an app element is a major interruption.
It's not fun awkwardly squeezing in a @tt{<script>} or custom element
in an otherwise concise notation.

@section{(Sloppily) Merging Distributions}
Let's create two Polyglot projects in a directory.
One follows @secref{default-workflow}, one follows @secref{functional-workflow}.

@verbatim[#:indent 4]|{
$ mkdir website && cd website
$ polyglot start old
$ polyglot start -f new
}|

These workflows are incompatible because an app element in one workflow will
produce no content in the other. Your options are to port code from one to the
other, or integrate the workflows. We'll do the latter, because porting code
can be time-consuming.

In @tt{new/assets/index.md}, add @litchar{[Other
site](other/index.html.literal)} wherever you'd like. From there you can
combine the websites by moving the imperative project's distribution into the
functional project's distribution.

@verbatim[#:indent 4]|{
$ polyglot build old
$ mv old/dist new/dist/other
}|

This works, but don't do this in your own projects. If the old site decides to
add its own link back to the new site, it would need to decide if it should
prefix each link with @litchar{..}  depending on if it appears in the
functional site. The maintenance burden is not worth it. I only show you this
to illustrate two benefits:

@itemlist[#:style 'ordered
@item{You can use a new workflow at any time.}
@item{You can control how distributions are combined to avoid name conflicts.}
]

@section{Aggregating Projects}
Anything can be an asset for a Polyglot project, including other Polyglot projects.
Therefore, one way to handle multiple workflows is to aggregate projects.

If you run these commands you'll end up with three projects, where the @tt{aggregate}
project hosts the other two.

@verbatim[#:indent 4]|{
$ polyglot start -f aggregate
$ cd aggregate/assets
$ polyglot start -f design
$ polyglot start legacy
}|

The commands imply there are now two projects following
@secref{functional-workflow}, but they can still evolve independently. We'll
use the aggregate project to set rules for the combined distribution, such that
@tt{polyglot build aggregate} bundles all three projects into a single
distribution. Here's one way to do that:

@racketmod[#:file "aggregate/.polyglotrc.rkt"
racket

(require polyglot)
(provide polyglot+%)

(define polyglot+%
  (class polyglot/functional% (super-new)
    (define design-path (assets-rel "design"))
    (define legacy-path (assets-rel "legacy"))
    (define build-legacy! (make-polyglot-builder legacy-path))
    (define build-design! (make-polyglot-builder design-path))

    (define/private (compile-child! name project-path build! changed removed)
      (define dist (build! #:changed changed #:removed removed))
      (define destination (dist-rel name))
      (define source (parameterize ([polyglot-project-directory project-path]) (dist-rel)))
      (delete-directory/files destination #:must-exist? #f)
      (copy-directory/files source destination)
      dist)

    (define/override (compile! #:changed [changed '()]
                               #:removed [removed '()])
      (define aggregate-dist (super compile! #:changed changed #:removed removed))
      (define legacy-dist (compile-child! "old" legacy-path build-legacy! changed removed))
      (define design-dist (compile-child! "new" design-path build-design! changed removed))
      (make-immutable-hash (append (dict->list aggregate-dist)
                                   (dict->list legacy-dist)
                                   (dict->list design-dist))))))
]

In this example, @racket[make-polyglot-builder] manages builds for the the @tt{legacy}
and @tt{design} projects. By overriding @method[unlike-compiler% compile!], we kick
off a build for each as a side effect of building the aggregate project. The distribution
for each child project is copied into the parent on completion, resulting in a final
distribution that looks like this:

@verbatim[#:indent 2]|{
dist/
  index.html
  old/
    index.html
  new/
    index.html
}|

One thing that's nice about this approach is that it supports live builds. If you
run @tt{polyglot develop aggregate} and change an asset in the @tt{design} project,
only the impacted assets are rebuilt.

This is still not an optimal arrangement because the websites are built in
sequence, and the distribution of each website is copied to the aggregate
distribution on every build.  From here you could modify this example to use
@secref["places" #:doc '(lib "scribblings/reference/reference.scrbl")] and/or
use symbolic links to child distributions to reduce disk activity.
