#lang scribble/manual

@require[@for-label[racket polyglot unlike-assets]]

@title[#:tag "hook-before-after"]{How-To: Hook Before and After Builds}

Under the hood, Polyglot workflows are classes. Each one is a subclass
of @racket[unlike-compiler%]. An instance of @racket[unlike-compiler%]
models relationships between files that otherwise don't fit
together. You can use it to make a PNG depend on an XML file, if you
know a way to have that make sense. Knowing the basics of the
@racketmodname[unlike-assets] collection can help you customize
Polyglot later.

The @method[unlike-compiler% compile!] method actually executes the
given workflow and the underlying build. This method is synchronous,
so if you want to add code to occur before or after a build, simply
override @method[unlike-compiler% compile!] and place code around
the superclass call.

@racketblock[
(class polyglot/functional%
  (define/override (compile! #:changed c #:removed r)
    (displayln "before build")
    (define output (super compile! #:changed c #:removed r))
    (displayln "after build")
    output))]
