#lang scribble/manual
@require[@for-label[
  polyglot
  txexpr
  unlike-assets/logging
  racket/base
  racket/contract]]

@title{Writing Custom Workflows}

If @racket[polyglot]'s default workflow does not suit your needs, then you can write your own.

@defproc[(run-txexpr! [tx-expressions (non-empty-listof txexpr?)]
                      [initial-layout (-> (listof txexpr?) (or/c txexpr? (listof txexpr?)))
                                      identity])
                      (or/c (listof txexpr?) txexpr?)]{
Runs @racket[tx-expressions] as if they were a collection of interdependent Racket modules.
Assumes that @racket[tx-expressions] contains representations of application
and library elements according to @secref{rackdown}.

Note that you do not have to use Markdown before using this function, or even
tagged expressions that represent valid HTML. You only need to represent
application or library @racket[<script>] elements as tagged X-expressions.

This will functionally replace Racket application and library elements inside the
provided tagged X-Expressions and return the expanded expressions within a layout.

Remember from the rules of @secref{rackdown} that the application elements within the
@racket[tx-expressions] are the authority of their own layout, but you may specify
an @racket[initial-layout] that will apply if the application elements don't supply one.

This procedure has side-effects:

@itemlist[
@item{Unique and short-lived directories and Racket modules will appear in @racket[(system-temp-rel)] according to @secref{rackdown}. They are deleted by the time control leaves this procedure.}
@item{Events will appear on @racket[unlike-assets-logger]. Info-level events will report summarized content fragments created by your application elements.  Any output on @racket[(current-error-port)] caused by script elements will apperar as error-level events.}
]

Note that this procedure does not do anything with dependencies. Let's get you equipped for that, too.}

@defproc[(discover-dependencies [tx txexpr?]) (listof string?)]{
Returns the values of @racket[href] or @racket[src] attributes in
@racket[tx] that appear to refer to assets on a local file system.
This will check for complete paths, relative paths, and URLs with
the @racket["file://"] scheme.

Relative paths will not be made complete. It's up to you to decide a base directory.
This frees you from needing to use @racket[(assets-rel)].
}

@defproc[(apply-manifest [tx txexpr?]
			 [manifest dict?])
			 txexpr?]{
Returns a new @racket[txexpr] such that all @racket[href] and
@racket[src] attribute values that appear as keys in @racket[manifest]
are replaced with the values in @racket[manifest]. Pair this with
@racket[discover-dependencies] to set up a workflow where discovered
build-time assets are replaced with production-ready assets.

@racketblock[
(define page (run-txexpr! (parse-markdown md-file) layout))

(define optimized (foldl (λ (dep res)
                           (dict-set res dep (write-optimized-to-disk! dep)))
			 #hash()
			 (discover-dependencies page)))

(code:comment "Replace things like <img src=\"logo.png\" /> with <img src=\"809a2d.png\" />")
(define production-ready (apply-manifest page optimized))

(with-output-to-file "page.html"
  #:exists 'truncate
  (λ ()
    (displayln "<!DOCTYPE html>")
    (displayln (xexpr->html page))))
]
}

Using all of the above procedures, you can subclass @racket[polyglot%] and provide
your own processing for data containing mixed-mode Racket elements.