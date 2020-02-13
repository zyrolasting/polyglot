#lang scribble/manual
@require[@for-label[racket
                    polyglot
                    [except-in markdown xexpr->string]
                    unlike-assets]
         "../macros.rkt"]

@title[#:tag "functional-workflow"]{The Functional Workflow}

In the functional workflow, each application element can provide a
@racket[replace-page] procedure to replace the entire page without
side-effects.

This example uses @racket[tx-replace] to replace the @tt{<head>} element with a
more usable starting point.

@margin-note{Notice that the replacement is a @italic{list} containing a @tt{head} element. This is because you can replace one element with several.}
@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base
(require polyglot)
(provide replace-page)

(define (replace-page page-tx)
  (tx-replace page-tx
              (λ (x) (tag-equal? 'head x))
              (λ (x) `((head ((title "My Page"))
                             (link ((rel "stylesheet") ("styles.css"))))))))]

So what's @tt{page-tx}? Given a Markdown file @tt{my-page.md}, the page
starts life as this value:

@racketblock[
 `(html (head (title "Untitled")) (body . ,(parse-markdown "my-page.md")))]

The @tt{replace-page} procedure provided by the @italic{first} app
element of @tt{my-page.md} will get this value bound to @racket[page-tx]. This
implies that the app element can "see itself" inside the page.

If an app element does not provide a @tt{replace-page} procedure, then it does
not change the page at all.

All app and lib elements are removed from the page they produced once they are
used, but the workflow will always look for new app elements that appear in new
versions of a page. The page is only considered "done" when there are no more
app elements left to replace the page. You can take advantage of this to do all
sorts of neat things.

@section{Multiple App Elements}

You may write other application elements in the page, and they will
be evaluated from top-to-bottom. The output of the first app element's
@tt{replace-page} procedure will be the input to the second app element's
@tt{replace-page} procedure, and so on.

This page counts to @racket[2].

@verbatim[#:indent 2]|{
Here is a number: <output>0</output>

<script type="text/racket" id="nums">
#lang racket/base
(require polyglot)

(define (increment page-tx)
  (tx-replace page-tx
              (λ (x) (tag-equal? 'output x))
              (λ (x) (number->string (add1 (string->number (car (get-elements x))))))))
</script>

<script type="application/racket">
#lang racket/base
(require "nums.rkt")
(provide (rename-out [increment replace-page]))
</script>

<script type="application/racket">
#lang racket/base
(require "nums.rkt")
(provide (rename-out [increment replace-page]))
</script>
}|

If there is one more app element left in a page, and that element create a new
page with even more app elements, then the new app elements are processed
without interruption.

@section{In-Place Replacements}
Use @racket[tx-replace-me] to replace the app element in which the procedure appears.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot)
(provide replace-page)

(define (replace-page page-tx)
  (tx-replace-me page-tx
                 (λ (x) `((p "I'm where the app element was.")))))]


@section[#:tag "boilerplate"]{Pre-processing and Post-processing}
Here's an app element that sets a layout for a page.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot
         "project/layouts.rkt")
(provide replace-page)
(define (replace-page page-tx)
  (one-column "My Page" page-tx))]

@margin-note{If you didn't read @secref{xml-html5-caveat}, do so before you go
wild with what I'm suggesting here.}
Wouldn't it be better to just write this?

@verbatim[#:indent 2]|{
<page title="My Page" layout="one-column" />
}|

To do this, go to the functional workflow in your @tt{.polyglotrc.rkt}
and override @method[polyglot/functional% preprocess-page]:

@racketmod[#:file ".polyglotrc.rkt"
racket/base
(provide polyglot+%)
(require polyglot)

(define polyglot+%
  (class* polyglot/functional%
    (define/override (preprocess-page page-tx)
      (tx-replace-tagged page-tx
                         'page
                         (λ (x)
                           `((script ((type "application/racket")
                                      (id ,(genid page-tx)))
                                     "#lang racket/base"
                                     "(require \"project/layouts.rkt\")"
                                     "(provide replace-page)"
                                     "(define (replace-page x)"
                                     ,(format "  (~a ~e x))"
                                              (attr-ref x 'layout "one-column")
                                              (attr-ref x 'title "Untitled")))))))))]

Now for every page, the non-standard @tt{<page>} element is replaced by an app
element you no longer have to write yourself.

To post-process each page, override @method[polyglot/functional% postprocess-page].
It behaves like every other page replacement call.
