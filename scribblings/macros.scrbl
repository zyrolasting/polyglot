#lang scribble/manual

@require[@for-label[racket/base racket/class txexpr polyglot]]

@title[#:tag "polyglot-macros"]{Page Macros and Preprocessing}

Racket macros replace Racket code inside of application elements and libraries.
However, they do not operate on the page containing the elements themselves.

Consider an application element that only sets the layout of the page.

I omit the @racket[<script>] markup for brevity.

@racketmod[racket/base
(require "project/assets/layouts.rkt")
(provide layout)
(define layout (lambda (kids) (two-column "My Page Title" (nav-layout) kids)))
]

This is only 4 lines of code (6 if you count the @tt{<script>} tags),
but it must duplicate for every page. This is especially tedious if all
you want to do is change the title.

A macro @italic{could} expand to this pattern of @racket[require], @racket[provide],
and @racket[define], but that macro has to come from somewhere. If you @racket[require]
that macro, the module path would still repeat across pages.

@racketmod[racket/base
(require "project/assets/macros.rkt")
(layout (two-column "My Page Title" (nav-layout) kids))
]

If you bundle the macro as a binding in a new language,
you still have the surrounding @racket[<script>] markup
eating up bytes and your precious writing time.

@verbatim[#:indent 2]{
<script type="application/racket">
#lang layout
(two-column "My Page Title" (nav-layout) kids)
</script>
}

To get around this, @racket[polyglot] can 
match and replace elements before processing
script elements.

@section{Replace elements using @tt{data-macro}}

Let's change the above example to use an element
with a @tt{data-macro} attribute.

@verbatim[#:indent 2]{
<meta itemprop="build-time"
      data-macro="set-layout"
      data-title="My Page Title">
}

@margin-note{Why @tt{<meta itemprop>}?
@itemlist[
@item{HTML5 allows it in a @tt{<body>}}
@item{It does not impact the appearance of a page when viewed by a human, but still adds meaning for a program.}
@item{@tt{meta} is a void element, so you don't have to type a closing tag.}]}

I use a @tt{<meta itemprop>} pattern here, but the element does not matter.
@racket[polyglot] responds by trying to load the module at
@racket[(assets-rel "set-layout.rkt")] and running a provided
@racket[replace-element] procedure. We'll assume this implementation
is handy.


@racketmod[#:file "set-layout.rkt"
racket/base

(provide replace-element)

(require txexpr)

(define (replace-element target)
  (define page-title (attr-ref target 'data-title))

  (code:comment "polyglot will add new line characters for us.")
  (code:comment "NOTE: This is a LIST containing a script element!")
  `((script ((type "application/racket"))
    "#lang racket/base"
    "(require \"project/assets/layouts.rkt\")"
    "(provide layout)"
    "(define (layout kids)"
    ,(format "(two-column ~e (nav-layout) kids))" page-title))))
]

In this setup, @racket[target] is the entire @tt{<meta>} element
as a @racket[txexpr]. @racket[replace-element] will simply return
the layout-defining application element to take its place.

@italic{Take careful note that @racket[replace-element] is returning a list
containing only a @tt{script} element.} Like application elements, procedures
acting as macros can replace one element with many.

If you are dreading writing one file per macro, don't worry.
You can specify an expected provided identifier after the module
name. This behaves the same way. @racket[replace-element] is just
a default identifier to seek if none is specified.

@verbatim[#:indent 2]|{
<meta itemprop="build-time"
      data-macro="set-layout replace-element"
      data-title="My Page Title">
}|

@section{Pre-processing}

If you don't want to use @tt{data-macro}, you'll need your own matching procedure.

Subclass @racket[polyglot%] and override
@method[polyglot% preprocess-txexprs].

@racketblock[
(require polyglot txexpr)

(define polyglot+preprocessor%
  (class* polyglot% ()
    (super-new)
    (define/override (preprocess-txexprs txexprs)
      (for/list ([tx (in-list txexprs)])
        (define-values (new-content _)
          (splitf-txexpr
           (λ (x) (and (txexpr? x)
                       (equal? 'script)
                       (equal? (attr-ref x 'type #f) "appliaction/racket")))
           (λ (x) `(script ((type "application/racket"))
                           "#lang limited"
                           ,@(filter (λ (s) (not (string-contains? s "#lang")))
                                     (get-elements x))))))
        new-content))))
]

This makes @racket[polyglot] replace all application elements
with new application elements under a prescribed limited
language. This can be help with untrusted code.

If you want to specialize @racket[polyglot]'s preprocessing
and still leverage @tt{data-macro}, call @racket[(super preprocess-txexprs txexprs)]
in your overriding method.


@section{A Cautionary Tale About Invalid HTML}

@racket[polyglot]'s Markdown parser happens to handle
invalid elements, making code golf easy. But there's a
gotcha.

You could make the earlier @tt{data-macro} example use an undefined
@tt{<m>} element, for example.

@verbatim[#:indent 2]|{
<m data-macro="set-layout replace-element"
   data-title="My Page Title">
}|

If you have your own matcher, you can bring it all home.

@verbatim[#:indent 2]|{
<layout title="My Page Title">
}|

You can build this with @racket[polyglot], and you'll get output.

Only it won't work as expected because you forgot to close the elements,
XHTML style. If you add the missing @tt{/}s, it will work.

@verbatim[#:indent 2]|{
<m data-macro="set-layout replace-element"
   data-title="My Page Title" />
}|
@verbatim[#:indent 2]|{
<layout title="My Page Title" />
}|

XML rules? In @italic{my} HTML5?!

The Markdown parser will treat HTML5 void elements as self-closing,
but you must explicitly close anything you make up. If my opinion matters
here, I'd strongly suggest @bold{only using valid HTML5}.

For one thing, that is the document type that @racket[polyglot] delivers
to end-users.

Additionally, having valid HTML5 among Markdown helps keep your content
decoupled from @racket[polyglot]'s default workflow.

If you want to incorporate macro-like functionality among your prose
(e.g. Wordpress shortcodes), remember that @racket[polyglot] operates
on @racket[txexpr] values at its core.  You can always parse your own
prose language instead of Markdown.
