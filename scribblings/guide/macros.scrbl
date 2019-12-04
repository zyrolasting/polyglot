#lang scribble/manual

@require[@for-label[racket/base racket/class txexpr polyglot]]

@title[#:tag "polyglot-macros"]{Page Macros and Preproessing}

Racket macros replace Racket code inside of application elements and libraries.
However, they do not operate on the page containing the elements themselves.

Consider an application element that only sets the layout of the page.

I omit the @tt{<script>} markup for brevity.

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

To get around this, the imperative workflow can match and replace
elements before processing script elements.

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
@item{It does not impact the appearance of a page when viewed by a human, but
still adds meaning for a program.}
@item{@tt{meta} is a void element, so you don't have to type a closing tag.}]}

I use a @tt{<meta itemprop>} pattern here, but the element does not matter.
The imperative workflow responds by trying to load @tt{assets/set-layout.rkt}
and running a provided @racket[replace-element] procedure.

We'll assume this implementation is handy:

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

Here, @racket[target] is the entire @tt{<meta>} element as a Tagged
X-expression. @racket[replace-element] will simply return the layout-defining
application element to take its place.

@italic{Take careful note that @racket[replace-element] is returning a list
containing only a @tt{script} element.} Like application elements,
these procedures can replace one element with many.

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

Subclass @racket[polyglot/imperative%] and override
@method[polyglot/imperative% preprocess-txexprs].

@racketblock[
(require polyglot txexpr)

(define polyglot+preprocessor%
  (class* polyglot/imperative% ()
    (super-new)
    (define/override (preprocess-txexprs txexprs)
      (for/list ([tx (in-list txexprs)])
        (define-values (new-content _)
          (splitf-txexpr
           (λ (x) (and (txexpr? x)
                       (equal? 'script)
                       (equal? (attr-ref x 'type #f) "application/racket")))
           (λ (x) `(script ((type "application/racket"))
                           "#lang limited"
                           ,@(filter (λ (s) (not (string-contains? s "#lang")))
                                     (get-elements x))))))
        new-content))))
]

This makes the imperative workflow replace all application elements with new
application elements under a prescribed limited language. This can be help with
untrusted code.

If you want to specialize the imperative workflow's preprocessing and still
leverage @tt{data-macro}, call @racket[(super preprocess-txexprs txexprs)] in
your overriding method.
