#lang scribble/manual
@require[@for-label[polyglot-reactive
                    racket/base]]

@title{polyglot-reactive}
@author{Sage Gerard}

@defmodule[polyglot-reactive]

This module offers an alternative implementation to Polyglot based on
@racketmodname[unlike-assets/reactive]. The @racketmodname[polyglot]
module leverages this package in a backwards-compatible way.


@defproc[(start-build-system! [maybe-makers (listof (-> string? procedure? (or/c #f live-build?)))])
                              u/a-build-system?]{
Starts a centralized @racketmodname[unlike-assets/reactive] build system
}
