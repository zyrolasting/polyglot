#lang scribble/manual
@require[@for-label[polyglot
                    txexpr
                    [except-in markdown xexpr->string]
                    unlike-assets
		    racket/rerequire
                    racket/base
                    racket/dict
                    racket/class]]

@title[#:tag "functional-workflow"]{The Functional Workflow}
@defmodule[polyglot/functional]

The functional workflow has less room for error and
continued flexibility as your project grows. The drawback
is a higher learning curve.

@section{Page Construction}
Given a Markdown file @tt{my-page.md}, the output content
of the functional workflow is:

@racketblock[
((compose A_N ... A_1 A_0)
 `(html (head (title "Untitled")) (body . ,(parse-markdown "my-page.md"))))]

Where @tt{A@subscript{0}}, @tt{A@subscript{1}}, ..., @tt{A@subscript{N}},
are @tt{replace-page} procedures @racket[provide]d by
the app elements defined within @tt{my-page.md}.

If an app element does not @racket[(provide replace-page)]
to act as @tt{A@subscript{i}}, then @tt{A@subscript{i}} will
be the identity function.

@section{Functional App Elements}
Since any functional app element may replace the entire page,
changing the page layout, adding new styles, or decorating
existing elements are all a matter of find/replace.

The following example replaces the @tt{<head>} element with a
more usable starting point.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base
(require polyglot)
(provide replace-page)

(define (replace-page page-tx)
  (replace-tagged page-tx
                  'head   
                  (λ (x) `(head (title "My Page")
                                (link ((rel "stylesheet") ("styles.css")))))))]

To elaborate on the @racket[compose] rule above, an app's
element's @tt{replace-page} procedure always accepts
the latest version of the page. This means the first app
element gets the bare bones page, and the second app element
gets the output from the first app element, and so on.

@margin-note{Notice that the @racket[λ] is returning a @italic{list} containing a @tt{head} element. This is because you can replace one element with several.}

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot)
(provide replace-page)

(define (replace-page page-tx)
  (replace-tagged page-tx
                  'head   
                  (λ (x) `((head (title "My Page")
                                (link ((rel "stylesheet") ("styles.css"))))))))]

@section{In-Place Replacements}

One of the benefits of the imperative workflow was how easy it is
to make an app element replace itself with new content: Just @racket[write].

In the functional workflow, you are responsible for locating and replacing
an app element's source. To make this easier, @racketmodname[polyglot/functional]
provides a parameter that always resolves to a matching predicate for the currently
evaluating app element's @racket[txexpr].

@racket[polyglot] uses a variable-pass model where it will always
replace app elements so long as they exist. To avoid an infinite loop,
@racket[polyglot] will always remove the exact app elements it runs
during a pass. You can exploit this to create procedures that behave
similarly to macros, or element-level loops.

@subsection{Example: Embed Working Code Examples}
This example generates a new application element with its own
source visible. The output page will have both the literal code
available for end-user reading, and the output of that same code
displayed right below it.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot)
(provide replace-page)

(define code '("#lang racket/base"
               "(provide replace-page)"
               "(require polyglot)"
               "(define (replace-page page-tx)"
               "  (replace page-tx"
               "           (current-app-element-predicate)"
               "           (λ (x) '((h1 \"Hello, meta!\")))))"))

(define (replace-page page-tx)
  (replace-me page-tx
              (λ (x) `((pre . ,code)
                       (script ((type "application/racket")) . ,code)))))]

@subsection{Example: Iterative App Element}
An app element can replace itself with a slightly different version of itself.
That way @racket[polyglot] won't remove the new version, and the new version
can pick up a computation in terms of the last iteration.

Obviously, this comes with a risk of non-terminating builds.

@racketmod[#:file "<script type=\"application/racket\">...</script>"
racket/base

(require polyglot)
(provide replace-page)

(define (replace-page page-tx)
  (replace page-tx
           (current-app-element-predicate)
           (λ (x)
             (define i (string->number (attr-ref 'data-i x "0")))
             (if (< i 10)
                 `(,(attr-set x 'data-i (number->string (add1 i))))
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
  (replace page-tx
           (current-app-element-predicate)
           (λ (x)
             (define-values (i o) (make-pipe))
             (parameterize ([current-output-port o])
               (dynamic-require "loud-module.rkt" #f)
               (close-output-port o))
             (port->list read i))))]

@section{Pre- and Post-processing}
You may notice that you are writing the same app elements over and over
again, even when leveraging shared code. This can happen when specifying
layouts, for example.

For this reason you can install @tt{replace-page} procedures to run
before or after @racket[polyglot] transforms a page using app elements.

Since @racket[(assets-rel "index.md")] is always the first page to run,
it is the best place to install new handlers. Astute readers might
ask how @tt{index.md} could benefit from the same handlers if they
are not installed by the time @tt{index.md} is already being processed.
Put simply, @tt{index.md} is the only page that has to preprocess itself.

Here's an @tt{index.md} that uses a custom @tt{<page />} XML element to specify
its own title and layout. The author wishes to use non-standard elements because
the Markdown parser will still parse them, and it beats typing an app element
to set up the page for every page.

@verbatim|{
<page title="Home Page" layout="holy-grail" />
<script type="application/racket" id="install-preprocessor">...</script>
}|

The @tt{<page />} element doesn't mean anything yet. @tt{index.md} must
use an app element install a pre-processor that replaces all @tt{<page />}
elements with app elements that configure the page.

All it needs to do is set @racket[current-page-preprocessor] and
@racket[current-page-postprocessor]. To address the prior lack of preprocessing
for itself, it will need to invoke its own pre-processor. The post-processor
will still run on @tt{index.md} by the time all app elements in the page finish.

@racketmod[#:file "<script id=\"install-preprocessor\">"
racket/base
(provide replace-page)
(require polyglot)

(define (preprocess page-tx)
  (replace-tagged page-tx
                  'page
                  (λ (x)
                    `((script ((type "application/racket"))
                              "#lang racket/base"
                              "(require \"project/layouts.rkt\")"
                              "(provide replace-page)"
                              "(define (replace-page x)"
                              (format "  (~a ~e x))"
                                      (attr-ref x 'layout)
                                      (attr-ref x 'title)))))))

(define (replace-page page-tx)
  (current-page-preprocessor preprocess)
  (preprocess page-tx))]

But what about post-processing? When would that be useful?

@subsection{Bundling Assets}
Take CSS and JavaScript. As assets, both are difficult to manage
from a delivery standpoint. Developers want to use both
in a way that's convenient for them, and let a build system
figure out how to optimize a soup of related files, CDATA, and remote
references for end-users.

For an example, here's an app element that prepends page-level CSS
to a designated element. The developer's premise is that it's painful
to write styles in a central vanilla CSS file, and would rather toss relevant
styles into the page using theme information and the ever-helpful @racket[css-expr].

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
  (replace
   page-tx
   (λ (x) (and (txexpr? x)
               (equal? (attr-ref x 'id #f)
                       "style-pile")))
   (λ (x) `(,(style
              ,(get-attrs x)
              ,(cons page-styles (get-elements x)))))))]

On its own, this approach bloats the output HTML by
including a @tt{<style>} element in the output.

That might not be a problem initially, but its possible
that other pages doing the same thing might grow enough
to cause problems. End-user browsers can't cache HTML
responses if you want them to stay up to date with styles,
so the stylesheets may need to move to a seperate file.

Heading back to @tt{index.md}, we can install a post-processor
that consumes the aggregate style elements and writes them
to the distribution. It uses @racket[current-seconds] to
create a basic cache-busting name.

@racketmod[racket/base
(provide replace-page)
(require polyglot)

; [...]

(define (postprocess page-tx)
  (replace page-tx
           (λ (x) (and (txexpr? x)
                       (equal? (attr-ref x 'id)
                               "style-pile")))
           (λ (x)
             (display-to-file #:exists 'replace
                              (string-join (get-elements x))
                              (dist-rel (string-append (number->string (current-seconds))
                                                       ".css")))
              null)))

(define (replace-page page-tx)
  (current-page-preprocessor preprocess)
  (current-page-postprocessor postprocess)
  (preprocess page-tx))
]

Post-processors give you the ability to form an agreement with your team
on how to use a page as a container for distributed data for later optimization
like this.

@section{Functional Workflow API}
@defclass[polyglot/functional% unlike-compiler% ()]{
In the terminology of unlike-assets, @racket[polyglot/functional%] uses complete paths as @racket[clear/c] names.

@defmethod[(clarify [unclear unclear/c]) clear/c]{
If the string looks like a readable path on your system, returns a complete path.
Relative paths are completed using @racket[(assets-rel)]. Complete paths are used as-is.
If the completed path path does not refer to a readable file, this will raise @racket[exn:fail].
}

@defmethod[(delegate [clear clear/c]) unlike-asset/c]{
This implements the workflow on this page by offering
special processing for Markdown dependencies with fallback processing
for any other file. Unlike @secref{default-workflow}, there is no added benefit
in offering Racket module processing.

This workflow will process any referenced Markdown files
to produce a working, linked collection of @racket[".html"] files.

Any other files you reference are copied to @racket[(dist-rel)],
such that the file name is the first eight characters of the SHA1 hash of the
file content for cache busting.
}
}
