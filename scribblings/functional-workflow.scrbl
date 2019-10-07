#lang scribble/manual
@require[@for-label[polyglot
                    txexpr
                    [except-in markdown xexpr->string]
                    unlike-assets
                    racket/base
                    racket/class
                    racket/dict
                    racket/file
		    racket/rerequire]]

@title[#:tag "functional-workflow"]{The Functional Workflow}
@defmodule[polyglot/functional]

The functional workflow offers a lean, predictable and scalable API
for building web pages. In it, each application element provides a
procedure to produce a new page. A finished page comes from
applying the composition of these procedures to the simplest
representation of the page.

@section{Variable-Pass Model}

The functional workflow runs on a loop.
Each pass performs the following steps:

@itemlist[#:style 'ordered
          @item{Find all app and lib elements}
          @item{Assign unique values to the @tt{id} attributes of each element where they don't exist.}
          @item{Write the CDATA of each element to a Racket module in @racket[system-temp-rel].}
          @item{Run each app element's Racket module according to @secref{func-app-el}}
          @item{If any substitutions occurred in this process, go to step 1.}]

This can create an infinite loop if, say, an application element
always creates more application elements in the page. To avoid
lockup, the workflow will always remove the exact app and lib
elements processed before concluding a pass. However, you can
exploit the system to create "macros", or loops using page elements.

@section[#:tag "func-app-el"]{Functional App Elements}
Any functional app element may replace the page.
This means the app can change the page layout, add new styles,
or decorate existing elements as a matter of find/replace.

The following example replaces the @tt{<head>} element with a
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


So what's @tt{page-tx}?

Given a Markdown file @tt{my-page.md}, every page starts with this value:

@racketblock[
 `(html (head (title "Untitled")) (body . ,(parse-markdown "my-page.md")))]

The @tt{replace-page} procedure provided by the @italic{first} app
element of @tt{my-page.md} will get this value as an argument. This
implies that the app element can "see itself" inside the page. The app
element may remove or modify itself to produce different behaviors.

If an app element does not provide a @tt{replace-page} procedure, then
it is assumed to provide the identity function. Meaning that by default,
app elements replace the page with itself, producing no changes.

You may write other application elements in the page, and they will
be evaluated as they are encountered. The output of the first app element's
@tt{replace-page} procedure will be the input to the second app element's
@tt{replace-page} procedure, and so on.

To illustrate, this outputs a page showing @racket[2].

@verbatim[#:indent 2]|{
<script type="text/racket" id="nums">
#lang racket/base
(require polyglot)
(provide replace-number)

; Turns <p>1</p> into <p>2</p>
(define (inc-child x)
  (number->string (add1 (string->number (car (get-elements x))))))

(define (replace-number page-tx)
  (tx-replace-tagged page-tx
                    'p
                    (λ (x) `((p ,(inc-child x))))))
</script>

<script type="application/racket" id="start">
#lang racket/base
(provide replace-page)
(define (replace-page page-tx)
  '(html (head (title "counter"))
         (body (p "0"))))
</script>

<script type="application/racket" id="first">
#lang racket/base
(provide replace-page)
(require "nums.rkt")
(define replace-page replace-number)
</script>

<script type="application/racket" id="second">
#lang racket/base
(provide replace-page)
(require "nums.rkt")
(define replace-page replace-number)
</script>
}|

We cover how to reduce boilerplate in the @secref{boilerplate} section.

@section{In-Place Replacements}
One of the benefits of the imperative workflow was how easy it is
to make an app element replace itself with new content: Just @racket[write].

In the functional workflow, use @racket[tx-replace-me] to replace
the element in which the procedure appears.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot)
(provide replace-page)

(define (replace-page page-tx)
  (tx-replace-me page-tx
                 (λ (x) `((p "I'm where the app element was.")))))]


@subsection{Example: Embed Working Code Examples}
This example generates a new application element with its own
source visible. The output page will have both the literal code
available for end-user reading, and the output of that same code
displayed right below it.

The reason this works is because the functional workflow will perform
another pass, see a new application element following a @tt{pre} element,
and then run it as well.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot)
(provide replace-page)

(define code '("#lang racket/base"
               "(provide replace-page)"
               "(require polyglot)"
               "(define (replace-page page-tx)"
               "  (tx-replace-me page-tx"
               "              (λ (x) '((h1 \"Hello, meta!\")))))"))

(define (replace-page page-tx)
  (tx-replace-me page-tx
                 (λ (x) `((pre . ,code)
                          (script ((type "application/racket")) . ,code)))))]

@subsection{Example: Iterative App Element}
The functional workflow tracks app and lib elements by their @tt{id} attribute
values. An app element can therefore replace itself with a slightly different
version with a new ID to avoid removal before the new pass.

Obviously this comes with a risk of non-terminating builds, but you can
use this to implement iteration. The following example only modifies
the @tt{id} attribute to illustrate the point, and is not an endorsement
to use the @tt{id} attribute as an ordinal for loops in general.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot)
(provide replace-page)

(define (replace-page page-tx)
  (tx-replace-me page-tx
                 (λ (x)
                   (define i (string->number (attr-ref 'id x "0")))
                   (if (< i 10)
                       `(,(attr-set x 'id (number->string (add1 i))))
                       `((p "done"))))))]

@subsection{Example: Incorporating Output from Side-Effects}
Another advantage of @secref{default-workflow} was that it naturally
integrated with @secref["printing" #:doc '(lib "scribblings/reference/reference.scrbl")].
This meant that if you use a graphical @tt{#lang} that used @racket[write] under the hood,
it will appear with little additional work.

In the functional workflow, you will need to capture output before
incorporating it into the page.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot)
(provide replace-page)

(define (replace-page page-tx)
  (tx-replace-me page-tx
                 (λ (x)
                   (define-values (i o) (make-pipe))
                   (parameterize ([current-output-port o])
                     (dynamic-require "loud-module.rkt" #f)
                     (close-output-port o))
                   (port->list read i))))]

@section[#:tag "boilerplate"]{Reducing Boilerplate}
Application and library elements consist largely of
boilerplate code when we are trying to make small
adjustments like setting a layout and title. So
let's assume that instead of writing this
at the top of every page...

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot
         "project/layouts.rkt")
(provide replace-page)
(define (replace-page page-tx)
  (one-column "My Page" page-tx))]

...we write this:

@verbatim[#:indent 2]|{
<page title="My Page" layout="one-column" />
}|

To do this, extend the functional workflow in @tt{.polyglotrc.rkt}
to preprocess each page.

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
                                     (format "  (~a ~e x))"
                                             (attr-ref x 'layout "one-column")
                                             (attr-ref x 'title "Untitled")))))))))]

So what about post-processing?

@subsection{Example: Bundling Assets}
Optimal CSS and JavaScript can be difficult to deliver. Developers
want to use both in a way that's convenient for them, and let a build
system figure out how to optimize a soup of related files, CDATA, and
remote references for end-users.

For an example, here's an app element that prepends page-level CSS
to a designated element. The developer's premise is that it makes sense
to associate page styles with the page instead of editing a central
CSS file.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot
         css-expr
         "project/theme.rkt")
(provide replace-page)

(define page-styles
  (css-expr->css
   (css-expr
    [.plan #:margin (0 auto)
           #:background-color ,(palette 'dark)
           #:color ,(palette 'light)])))

(define (replace-page page-tx)
  (tx-replace
   page-tx
   (λ (x) (and (txexpr? x)
               (equal? (attr-ref x 'id #f)
                       "style-pile")))
   (λ (x) `(,(style
              ,(get-attrs x)
              ,(cons page-styles (get-elements x)))))))]

On its own, this approach bloats the output HTML by
including a @tt{<style>} element in the output.

That might not be a problem initially, but its possible that other
pages doing the same thing might grow too large to deliver the same
way.

We can also install a post-processor that writes the aggregated style
rules to the distribution, and removes them from the output HTML.

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
                                    (format "  (~a ~e x))"
                                            (attr-ref x 'layout "one-column")
                                            (attr-ref x 'title "Untitled")))))))

    (define/override (postprocess-page page-tx)
      (tx-replace page-tx
                  (λ (x) (and (txexpr? x)
                              (equal? (attr-ref x 'id)
                                      "style-pile")))
                  (λ (x)
                    (display-to-file #:exists 'replace
                                     (string-join (get-elements x))
                                     (dist-rel (string-append (number->string (current-seconds))
                                                              ".css")))
                    null)))))]


@section{Functional Workflow API}
@defclass[polyglot/functional% unlike-compiler% ()]{
In the terminology of @racketmodname[unlike-assets], @racket[polyglot/functional%] uses complete paths as @racket[clear/c] names.

@defmethod[(clarify [unclear unclear/c]) clear/c]{
If the string looks like a readable path on your system, returns a complete path.
Relative paths are completed using @racket[(assets-rel)]. Complete paths are used as-is.
If the completed path path does not refer to a readable file, this will raise @racket[exn:fail].
}

@defmethod[(delegate [clear clear/c]) unlike-asset/c]{
This implements the workflow on this page by offering
special processing for Markdown dependencies with
fallback processing for any other file. Unlike @secref{default-workflow},
there is no Racket module processing.

This workflow will process any referenced Markdown files
to produce a working, linked collection of @racket[".html"] files.

Any other files you reference are copied to @racket[(dist-rel)],
such that the file name is the first eight characters of the SHA1 hash of the
file content for cache busting.
}

@defmethod[(preprocess-page [page-tx txexpr?]) txexpr?]{
A page-replacing method that runs before any app element's @tt{replace-page} procedure.

By default, this is the identity function.
}

@defmethod[(postprocess-page [page-tx txexpr?]) txexpr?]{
A page-replacing method that runs after every app element's @tt{replace-page} procedure.

By default, this is the identity function.
}
}

@defproc[(run-txexpr/functional! [target (or/c (listof txexpr?)
                                               txexpr?)]
                                 [#:max-passes max-passes exact-integer? 1000])
                                 txexpr?]{
Applies the functional workflow to the tagged X-expression fragment or element.
Use if you want to benefit from the functional workflow without use of
the CLI or @racket[polyglot/functional%].

If the process would exceed @tt{max-passes}, it will raise @racket[exn:fail].
Returns a completed page produced by the Racket modules inside @tt{target}.
If no procedures apply to the @tt{target} and @tt{target} is a list of elements,
then it will be wrapped in a minimal HTML5 document structure like so:

@racketblock[`(html (head (title "Untitled")) (body . ,target))]

This procedure has the following side-effects (in addition to all side-effects
produced by app or library elements):

@itemlist[
@item{Unique and short-lived directories and Racket modules will appear in @racket[(system-temp-rel)] according to @secref{rackdown}. They are deleted by the time control leaves this procedure.}
@item{Events will appear on @racket[unlike-assets-logger].}
]
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
