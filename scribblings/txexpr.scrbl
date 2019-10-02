#lang scribble/manual
@require[@for-label[polyglot/txexpr
                    racket/base
                    racket/contract]
         polyglot/txexpr]

@title{Specialized Tagged X-Expression Procedures}
@defmodule[polyglot/txexpr]

This module includes all bindings from the @seclink["top" #:doc '(lib "txexpr/scribblings/txexpr.scrbl") "txexpr"] (unsafe) and @seclink["top" #:doc '(lib "xml/xml.scrbl") "xml"] modules, plus curated procedures for working with tagged X-expressions as if they were program state. Since this module is lower-level, none of these procedures operate with any understanding of any
documented workflow.

@defproc[(tag-equal? [tag symbol?] [tx any/c]) boolean?]{
Returns @racket[#t] if @racket[tx] is a tagged X-expression and its tag is @racket[equal?] to @tt{tag}.
}

@defproc[(substitute-many-in-txexpr [tx txexpr?]
                                    [replace? (-> txexpr-element? any/c)]
                                    [replace (-> txexpr-element? (listof txexpr-element?))])
                                    (values (or/c (listof txexpr-element?) txexpr?)
                                            (listof txexpr?))]{
@margin-note{Pay careful attention to the wording here.}
Find and replace @italic{all} elements in @tt{tx} with @italic{at least one} child
element matching @tt{replace?}. Each @italic{immediate descendent} element
@tt{C} is replaced with @italic{all} elements from @tt{(replace C)}. Returns the
new content as the first value, and a list of the reconstructed elements as the second value.

Normally you do not need to call this directly, but it is helpful to understand
how it works. This is useful if you want to build a "stepper" to inspect
replacements in a broader pipeline.

@racketinput[
(substitute-many-in-txexpr
  '(main (div (p "1") (p "2")))
  (λ (x) (tag-equal? x 'p))
  (λ _ '((b) (b))))
]
@racketblock[
'(main (div (p "1") (p "2")))
'()
]

A matching element can be replaced by @italic{at least zero} elements, so
the @tt{replace} procedure must return a list of @racket[txexpr]. Return an empty
list to remove the element outright (possibly leaving an empty parent element).

@racketinput[
(substitute-many-in-txexpr
  '(main (div (p "1") (p "2")))
  (λ (x) (tag-equal? 'p x))
  (λ _ null))]
@racketblock[
'(main (div))
'((div (p "1") (p "2")))
]

As a special case, if @tt{(replace? tx)} is true, then the return values
will be @racket[(values (replace tx) (list tx))]. This is the only case where
the first returned value matches @racket[(listof txexpr-element?)] and
not @racket[txexpr?] in the range contract.

@racketinput[
(substitute-many-in-txexpr
  '(main (div (p "1") (p "2")))
  (λ (x) (tag-equal? 'main x))
  (λ _ '((root))))]
@racketblock[
'((root))
'((main (div (p "1") (p "2"))))
]

Take care to understand that while @italic{all} elements with at least one
matching child are reconstructed, the substitution will not account for nested
children. This avoids the risk of infinite loops in the event replacement
elements always includes other matching elements.

To guarentee full replacement of elements, use @racket[substitute-many-in-txexpr/loop].

@racketinput[
(substitute-many-in-txexpr '(p "old" (p "old") "old")
                           string?
                           (λ _ '("new")))]
@racketresult['(p "new" (p "old") "new")]
@racketresult['((p "old" (p "old") "old"))]
}

@defproc[(substitute-many-in-txexpr/loop [tx txexpr?]
                                         [replace? (-> txexpr? any/c)]
                                         [replace (-> txexpr? (listof txexpr?))]
                                         [#:max-replacements max-replacements exact-integer? 1000])
                                         txexpr?]{
Repeats @racket[substitute-many-in-txexpr] until no substitutions
are possible. To illustrate, this would not terminate if it weren't for
@tt{max-replacements}:

@racketblock[
(substitute-many-in-txexpr/loop '(p)
                                (λ (x) (tag-equal? x 'p))
                                (λ (x) '((p))))
]

@racket[substitute-many-in-txexpr/loop] raises @racket[exn:fail] if
it iterates once more after performing @tt{max-replacements}.
}

@defproc[
(interlace-txexprs
 [tx-expressions (non-empty-listof txexpr?)]
 [replace?/list-or-proc (or/c (-> txexpr? any/c)
                        (non-empty-listof (-> txexpr? any/c)))]
 [replace/list-or-proc  (or/c (-> txexpr? (listof txexpr?))
                        (non-empty-listof (-> txexpr? (listof txexpr?))))]
 [#:max-replacements max-replacements exact-integer? 1000]
 [#:max-passes max-passes exact-integer? 50])
 (non-empty-listof txexpr?)
]{
@racket[interlace-txexprs] returns a list of tagged X-expressions
constructed by a variable number of @tt{passes} over @tt{tx-expressions}.
For each pass, the following happens:

@itemlist[#:style 'ordered
@item{For each @tt{replace?} and @tt{replace} procedure, apply @racket[(substitute-many-in-txexpr/loop (cons (gensym) tx-expressions) replace? replace #:max-replacements max-replacements)].}
@item{If any replacements occurred, repeat.}]

If @tt{replace?/list-or-proc} or @tt{replace/list-or-proc} are not lists,
they will be treated as if they were lists containing the original value as
the only element. The lists must have the same number of elements,
just like if you had provided them to @racket[map] or @racket[foldl].

@racket[interlace-txexprs] returns only the transformed list of tagged X-expressions.
}
