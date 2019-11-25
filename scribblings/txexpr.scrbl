#lang scribble/manual
@require[@for-label[polyglot
                    racket/base
                    racket/contract]
         polyglot]

@title[#:tag "polyglot-txexpr"]{Tagged X-Expression Tools}
@defmodule[polyglot/txexpr]

This module includes all bindings from the @seclink["top" #:doc '(lib
"txexpr/scribblings/txexpr.scrbl") "txexpr"] and @seclink["top" #:doc '(lib
"xml/xml.scrbl") "xml"] modules, plus procedures you can use to build advanced
workflows around tagged X-expressions. None of these procedures are dependent
on any built-in workflow.

@section{Analysis}
@defproc[(tx-search-tagged [tx txexpr?]
                           [tag symbol?])
                           (listof txexpr?)]{
Return all elements in @tt{tx} with the given tag. Returns an empty list
if there are no matches.

If an element matches, the search will not descend into the child elements.
}

@defproc[(tag-equal? [tag symbol?] [tx any/c]) boolean?]{
Returns @racket[#t] if @racket[tx] is a tagged X-expression and its tag is @racket[equal?] to @tt{tag}.
}

@defproc[(make-tag-predicate [tags (non-empty-listof symbol?)]) (-> any/c boolean?)]{
Returns a procedure that checks if a value causes @racket[tag-equal?] to return
@racket[#t] for any of the given @tt{tags}.
}

@defproc[(discover-dependencies [tx txexpr?]) (listof string?)]{
Returns the values of @racket[href] or @racket[src] attributes in
@racket[tx] that appear to refer to assets on a local file system.
This will check for complete paths, relative paths, and URLs with
the @racket["file://"] scheme.

Relative paths will not be made complete. It's up to you to decide a base directory.
This frees you from needing to use @racket[(assets-rel)].
}

@section{Replacing Elements}

The following procedures find and replace elements using callbacks. The callback
that defines replacements elements is called @tt{replace} for the sake of this
documentation.

The procedures are either @italic{passive} or @italic{aggressive}. The aggressive
procedures will replace all matching elements, including the ones that appear in
the replacements it already made. The passive procedures will replace all matching elements,
but if the replacement produces more matching elements, they will simply leave them in
the output.

Consider the following replacement rules:

@verbatim|{
<p title="Hi">...</p> --> <section><h1>Hi</h1><p>...</p></section>
           <p>...</p> --> <section><p>...</p></section>
}|

In this case you want a passive procedure, because it will replace all
@tt{<p>} elements in a document with @tt{<section>} elements, but will leave
the @tt{<p>} elements from the replacement as part of the intended output.

An aggressive procedure would not terminate because the substitution rules
would disallow any @tt{<p>} elements, even in the replacement.

@verbatim|{
<p title="Hi">...</p>
 --> <section><h1>Hi</h1><p>...</p></section
 --> <section><h1>Hi</h1><section><p>...</p></section></section>
 --> <section><h1>Hi</h1><section><section><p>...</p></section></section></section>
 ...
}|

This is not to say that aggressive procedures are generally unhelpful. They are
meant for use cases where replacements may vary and eventually stop producing
matching elements.

@subsection{Passive Replacement}

@defproc[(tx-replace [tx txexpr?]
                     [predicate (-> txexpr-element? any/c)]
                     [replace (-> txexpr-element? (listof txexpr?))])
                     txexpr?]{
Replaces each element @tt{E} matching a predicate with the list of elements
returned from @tt{(replace E)}. Note that this can create empty parent elements.
Acts as a shorthand for @racket[substitute-many-in-txexpr].
}

@defproc[(tx-replace-tagged [tx txexpr?]
                            [tag symbol?]
                            [replace (-> txexpr? (listof txexpr?))])
                            txexpr?]{
Like @racket[tx-replace], except you can designate all elements of a certain tag.

e.g. @racket[(tx-replace-tagged tx 'h2 (lamdba (x) `((h3 . ,(get-elements x)))))]
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

A matching element can be replaced by @italic{at least zero} elements, so
the @tt{replace} procedure must return a list of @racket[txexpr].

@racketinput[
(substitute-many-in-txexpr
  '(main (div (p "1") (p "2")))
  (λ (x) (tag-equal? 'p x))
  (λ _ '((b) (b))))
]
@racketblock[
'(main (div (b) (b) (b) (b)))
'((div (p "1") (p "2")))
]

Return an empty list to remove the element
outright (possibly leaving an empty parent element).

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
@racketblock[
'(p "new" (p "old") "new")
'((p "old" (p "old") "old"))
]
}

@defproc[(apply-manifest [tx txexpr?]
			 [manifest dict?]
                         [rewrite (-> string? string?) (lambda ...)])
			 txexpr?]{
Returns a new @racket[txexpr] such that each @racket[href] and @racket[src]
attribute value that appears as a key @tt{K} in @racket[manifest] is replaced
with @racket[(rewrite (dict-ref manifest K))]. By default, @racket[rewrite]
returns only the @racket[name] value returned from @racket[split-path].

Pair this with @racket[discover-dependencies] to set up a workflow where
discovered build-time assets are replaced with production-ready assets.

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


@subsection{Aggressive Replacement}

@deftogether[(
@defproc[(tx-replace/aggressive [tx txexpr?]
                                [predicate (-> txexpr-element? any/c)]
                                [replace (-> txexpr-element? (listof txexpr?))])
                                txexpr?]
@defproc[(tx-replace-tagged/aggressive [tx txexpr?]
                                [tag symbol?]
                                [replace (-> txexpr? (listof txexpr?))])
                                txexpr?])]{
Aggressive variants of @racket[tx-replace] and @racket[tx-replace-tagged].

Acts as a shorthand for @racket[substitute-many-in-txexpr/loop].
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
 [tx-expressions (or/c txexpr? (non-empty-listof txexpr?))]
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

Unlike the other substitution procedures, @racket[interlace-txexprs]
accepts multiple pairings of @tt{replace?} and @tt{replace}. If
@tt{replace?/list-or-proc} or @tt{replace/list-or-proc} are not lists,
they will be treated as if they were lists containing the original value as
the only element. The lists must have the same number of elements,
just like if you had provided them to @racket[map] or @racket[foldl].

For each pass, the following happens:

@itemlist[#:style 'ordered
@item{For each @tt{replace?} and @tt{replace} procedure, do this:
@racketblock[(substitute-many-in-txexpr/loop (cons (gensym) tx-expressions)
                                             replace?
                                             replace
                                             #:max-replacements max-replacements)]}
@item{If any replacements occurred, repeat.}]

@racket[interlace-txexprs] returns only the transformed list of tagged X-expressions,
or raises @racket[exn:fail] if it would exceed @tt{max-passes}.

This is the procedure you would likely use to write more flexible workflows.
Here is an example program that parses a Markdown file, and
defines a pass to remove all script and style elements, then
all elements with no children. Because the procedure
will continue until no substitutions are possible,
only the heading will remain.

@racketblock[
(require racket/list
         racket/string
         markdown
         polyglot/txexpr)

(define (discard . _) null)

(define md (string-join '("# Hello, world"
                          "<script>blah</script>"
                          "<b><i><br></i></b>")
                        "\n"))

(interlace-txexprs (parse-markdown md)
                   (list (make-tag-predicate '(script style))
                         (λ (x) (and (txexpr? x)
                                     (empty? (get-elements x)))))
                   (list discard
                         discard))
]
@racketblock[
'((h1 ((id "hello-world")) "Hello, world"))
]
}

@section{Content Generation}
@defproc[(genid [tx txexpr?]) string?]{
Returns a value for an @tt{id} attribute that is not used anywhere in @tt{tx}.
}
