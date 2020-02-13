#lang scribble/manual

@require[@for-label[racket
                    polyglot
                    (only-in markdown parse-markdown)
                    unlike-assets
                    unlike-assets/logging]]

@title{@tt{polyglot/functional}}

@defmodule[#:multi (polyglot/functional (submod polyglot/functional safe))]

To use module-level contracts, require the @racket[safe] submodule.

@defclass[polyglot/functional% polyglot/base% ()]{
Specializes @racket[polyglot/base%] to process Markdown files
where application elements can replace page contents without
side-effects.

@defmethod[(delegate [clear clear/c]) unlike-asset/c]{
Like @racket[polyglot/base%]'s implementation, except
Markdown files (identified by an @racket[#".md"] extension)
are handled with this @racket[advance/c] procedure:

@racketblock[
(λ (clear compiler)
   (define fragment (parse-markdown clear))
   (define base-page (make-minimal-html-page fragment))
   (define preprocessed (preprocess-page base-page))
   (add-dependencies!
        clear
        compiler
        (postprocess-page (run-txexpr/functional! preprocessed))))]
}

@defmethod[(preprocess-page [page-tx txexpr?]) txexpr?]{
A page-replacing method that runs before any app @tt{replace-page} procedure provided by an application element's module.

By default, this is the identity function.
}

@defmethod[(postprocess-page [page-tx txexpr?]) txexpr?]{
A page-replacing method that runs after every @tt{replace-page} procedure provided by an application element's module.

By default, this is the identity function.
}
}

@defproc[(run-txexpr/functional! [target (or/c (listof txexpr?)
                                               txexpr?)]
                                 [#:max-passes max-passes exact-integer? 1000])
                                 txexpr?]{
Transforms @racket[target] into a new tagged X-expression,
@racket[target-prime], presumably representing HTML5. If
@racket[target] is a list of elements, then this procedure will begin
processing with @racket[(make-minimal-html-page target)] instead.

The transformation is a fold on @racket[target] into a new page
@racket[target-prime] that repeats these steps until no substitutions occur:

@itemlist[#:style 'ordered
          @item{Save all Racket modules from @tech{application elements} and @tech{library elements} found in @racket[target-prime] to disk.}
          @item{Evaluate @racket[(dynamic-require path 'replace-page (lambda () (lambda (x) x)))] for each @racket[path] derived from an app element, in order.}
          @item{For each @racket[replace-page] procedure, functionally replace @racket[target-prime] with

            @racketblock[(parameterize ([current-replace-element-predicate F])
                           (replace-page target-prime))]

            where @racket[F] is a predicate that matches the application element that sourced the @racket[replace-page] procedure.}
          @item{Remove from @racket[target-prime] all app and lib elements discovered in Step 1.}]

If this process repeats more than @racket[max-passes] times, the
procedure will raise @racket[exn:fail].

In addition to all side-effects produced by app or library elements,
status events will appear on @racket[unlike-assets-logger]. Temporary
Racket modules are deleted by the time control leaves this procedure.
}

@defthing[current-replace-element-predicate (parameter/c (-> txexpr-element? any/c)) #:value (λ _ #f)]{
A parameter used to set the target for @racket[tx-replace-me]. The functional
workflow will set this parameter to a predicate that matches the application
element in which a @tt{replace-page} procedure is evaluating.
}

@defproc[(tx-replace-me [tx txexpr?]
                        [replace (-> txexpr? (listof txexpr?))])
                        txexpr?]{
Like @racket[tx-replace], except the predicate is @racket[(current-replace-element-predicate)].
}
