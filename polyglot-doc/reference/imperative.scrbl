#lang scribble/manual

@require[@for-label[racket
                    polyglot
                    (only-in markdown parse-markdown)
                    unlike-assets
                    unlike-assets/logging]]

@title{@tt{polyglot/imperative}}

@defmodule[polyglot/imperative]

The Imperative Workflow seeks application elements within Markdown
files and runs them under the expectation that they will produce
content as a side-effect. In that sense, app elements behave similarly
to @litchar{<?php echo ...; ?>} in PHP.

@defclass[polyglot/imperative% polyglot/base% ()]{
Implements the imperative workflow.

@defmethod[(delegate [clear clear/c]) unlike-asset/c]{
Like @method[polyglot/base% delegate], except Markdown files
(identified by a @racket[#".md"] extension) are parsed and processed
using this @racket[advance/c] procedure:

@racketblock[
(λ (clear compiler)
  (define txexpr/parsed (parse-markdown clear))
  (define txexpr/preprocessed (send compiler preprocess-txexprs txexpr/parsed))
  (define txexpr/processed (run-txexpr/imperative! txexpr/preprocessed))
  (add-dependencies! clear compiler txexpr/processed))]
}

@defmethod[(preprocess-txexprs [tx-expressions (listof txexpr?)]) (listof txexpr?)]{
This method transforms @racket[tx-expressions] parsed from a source Markdown file
into a new list of tagged X-expressions. This transformation occurs before
the instance uses @racket[run-txexpr/imperative!].

Use this to sanitize untrusted code, generate application elements based on content,
or attach common metadata to documents.

The default implementation searches @racket[tx-expressions] for elements with a
@tt{data-macro} attribute. If the attribute exists, it must be a string of at
most two words (e.g. @racket["holiday"] or @racket["holiday halloween"]).  If a
second word is not specified, it is assumed to be @racket["replace-element"].

The first word is converted to a path to a Racket module in the @tech{assets
directory}, and the second word is converted to a symbol used to extract a
provided identifier via @racket[dynamic-require], with reload support for live
builds (e.g. @racket[(dynamic-require (assets-rel "holiday.rkt") 'halloween)]).

The @racket[dynamic-require] must return a @racket[(-> txexpr? (listof
txexpr?))] procedure that transforms the original Tagged X-expression holding
the @tt{data-macro} attribute to at least zero new elements.
}
}

@defproc[(run-txexpr/imperative! [target (or/c txexpr? (non-empty-listof txexpr?))]
                                 [initial-layout (-> (listof txexpr?) (or/c txexpr? (listof txexpr?)))
                                                 identity])
                                 (or/c (listof txexpr?) txexpr?)]{
Transforms @racket[target] into a new tagged X-expression, @racket[target-prime], presumably representing HTML5.

The transformation does not mutate @racket[target], but does depend on side-effects:

@itemlist[#:style 'ordered
          @item{Remember @racket[initial-layout] as the page layout.}
          @item{Save all Racket modules from @tech{application elements} and @tech{library elements} found in @racket[target-prime] to disk. Remove all library elements from @racket[target-prime].}
          @item{For each Racket module @racket[M] written to disk, in order matching app elements encountered:
            @itemlist[#:style 'ordered
            @item{Instantiate the module using @racket[(dynamic-require M #f)]}
            @item{@racket[read] all tagged X-expressions produced by @racket[write] calls in the module as a side-effect.}
            @item{Replace the page layout with @racket[(dynamic-require M 'layout)], if possible. Otherwise keep the existing layout.}
            @item{Replace the app element that sourced @racket[M] with the content written by that element.}]}
          @item{Return @racket[(layout target-prime)], where @racket[layout] is bound to the layout procedure after Step 3.}]

Side-effects:

@itemlist[
@item{Unique and short-lived directories and Racket modules will appear in @racket[(system-temp-rel)]. They are deleted by the time control leaves this procedure.}
@item{Events will appear on @racket[unlike-assets-logger]. Info-level events will report summarized content fragments created by your application elements. Any output on @racket[(current-error-port)] caused by script elements will apperar as error-level events.}
]
}

@defthing[polyglot% class?]{
An alias for @racket[polyglot/imperative%] kept for backwards compatibility.}
@defthing[run-txexpr! procedure?]{
An alias for @racket[run-txexpr/imperative!] kept for backwards compatibility.}
