[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/subscribe.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/polyglot/index.html)
[![](https://tokei.rs/b1/github/zyrolasting/polyglot)](https://github.com/zyrolasting/polyglot)

`polyglot` creates websites using a mix of any DSLs you want in a single source file.


```
$ raco pkg install polyglot
$ raco docs polyglot
```

## I don't know how you managed it, but your README is broken.

It's a feature. No, really, the mess of text below is not an accident
(Although it is more readable in raw form).

This README file functions as a demo and a working
example of how to author a page using `polyglot`.

Once you have `polyglot` installed, run `raco polyglot demo` to verify your installation
and build this README. If it worked, you will see a `dist` folder in your working
directory. Open the HTML file inside in your browser. If it did not work, please
[open an issue](https://github.com/zyrolasting/polyglot/issues).

<script type="text/racket" id="my-components">
#lang racket/base

;;; This is a library module. Polyglot will write this file to a temp filesystem
;;; with a name matching the `<script>` id. Application scripts in this page can
;;; `require` embedded library modules.

;;; Procedures can act as Components (in the React sense)
(provide page-layout)

(define (page-layout content)
  `(html (head (title "Polyglot Demo")
               (meta ((charset "utf-8")))
               (meta ((name "viewport" content "initial-scale=1, maximum-scale=1, shrink-to-fit=no, user-scalable=no, width=device-width")))
               (style ((type "text/css"))
                  "body { margin: 1rem; background: #0f2753 } body > :not(section) { display: none } section { color: #fff } a {color: goldenrod}"))
         (body . ,content)))
</script>

<script type="text/racket" id="literal">
#lang racket
;;; Define DSL for expressing literal text
;;; From https://docs.racket-lang.org/guide/hash-lang_reader.html

(require syntax/strip-context)

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))

(define (literal-read-syntax src in)
  (with-syntax ([str (port->string in)])
    (strip-context
     #'(module anything racket
         (provide data)
         (define data 'str)))))
</script>


<script type="text/racket" id="freeform">
#lang reader "literal.rkt"
`polyglot` uses Markdown for prose and is represented internally as [tagged X-expressions.](https://docs.racket-lang.org/txexpr/index.html?q=txexpr)
You can drop into any language you want to author content in only the most fitting terms.

This system is for developers who want to write blogs and showcase applications within their content.
It's not always easy, but the flexibility is game-changing.

## What about `scribble` or `frog`? Why would I use this?

Both of these tools are the best at what they do in the Racket ecosystem. If one fits your needs, use it.

For my purposes I wanted a tool that:

1. Does not keep too many degrees of separation between the author and a common representation of content.
2. Allows the author to use any `#lang` language at a moment's notice among prose.
3. Models every page as self-contained and self-describing, meaning that a page's source defines both its own metadata and how to use it.
4. Allows the author to share content without strictly needing `polyglot`. Since every page is just Markdown, any Markdown parser can process it. Since Racket has a well-defined interface for reading new languages, it's not a huge lift to write an alternative for `polyglot` either.

I set these requirements to hit a sweet spot of writing arbitrarily rich content with prose by default,
without setting the conditions for vendor lock-in.

I made the development experience as "get up and go" as I could, but you can only make the
most of this project if you need to author content using different DSLs per-page. This is
great for, say, a technical portfolio where each page hosts an app demo.
</script>

<script type="application/rackdown" id="main">
#lang racket/base

;;; This is an application script.
;;; It can set the page layout using `(provide layout)` and write content to replace
;;; the enclosing script node with a sequence of tagged X-expressions using the Printer
;;; in write mode.

(require
  markdown
  unlike-assets/logging
  "my-components.rkt"
  (rename-in "freeform.rkt"
             (data embedded-markdown)))

(provide layout)

(write `(section
  (h1 "...What just happened?")
    "This document was expanded on your system using several languages coordinated by Racket. "
    "In this context, Markdown files are programs containing at least one DSL. "))

(write `(section . ,(parse-markdown embedded-markdown)))

(define layout page-layout)
</script>

[ua]: https://github.com/zyrolasting/unlike-assets
