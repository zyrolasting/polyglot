#lang scribble/manual

@title{Polyglot: A Guide for Building Websites With Any Language}
@author{Sage Gerard}

This document serves as an introduction to Polyglot, a tool that runs programs
within @seclink["top" #:doc '(lib "txexpr/scribblings/txexpr.scrbl") "tagged
X-expressions"] to build powerful websites.

This guide explains why Polyglot is different, and how to get started using
its built-in conveniences. Once you're ready, you can read the
@seclink["top" #:doc '(lib "polyglot/scribblings/reference/polyglot-reference.scrbl")]{API Reference},
and learn new tricks from @seclink["top" #:doc '(lib
"polyglot/scribblings/tutorials/polyglot-tutorials.scrbl")]{tutorials} and
@seclink["top" #:doc '(lib "polyglot/scribblings/how-tos/polyglot-how-tos.scrbl")]{how-tos}.

@table-of-contents[]
@include-section{motivation.scrbl}
@include-section{get-started.scrbl}
@include-section{racket-in-markdown.scrbl}
@include-section{base-workflow.scrbl}
@include-section{imperative-workflow.scrbl}
@include-section{functional-workflow.scrbl}
@include-section{now-what.scrbl}