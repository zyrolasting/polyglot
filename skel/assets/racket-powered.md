<script type="application/rackdown">
#lang racket/base
(require "project/vcomps.rkt")
(provide layout)
(define layout (λ (kids) (page "I Built This Website Using Racket. Here's What I Can Do Now." kids)))
</script>

_This is an excerpt from the announcement page for the project.
The original article can be found [here](https://sagegerard.com/racket-powered.html)._

It looks like there's not much to see here. Isn't it great?
Barring exceptional cases, every page on this website takes up fewer than
20 kibibytes on disk and runs no JavaScript. Throttle your connection and
hit refresh a few times. It loads in _milliseconds_. No one does that
anymore.

It's funny. I spent years learning the ins and outs of front-end
web development, only to feel _happy_ making a tiny, 90s-looking
page that loads in a blink of an eye with nothing to do but
make a point. Is that ironic? I haven't decided yet.

But I did not just write vanilla HTML and give up my shiny toys.
I use [Racket][racket] to shape content using any language I want.
What better tool for starting with a blank slate?

## A Website With Superpowers

Racket is a programming language programming language.
Racket allows you to write your own languages in terms of itself, and
it is a _joy_ to use. I released [`unlike-assets`][ua]&mdash;an open
source build tool that functions as an alternative to Webpack for my
purposes&mdash;to say thank you and to show off what it can do.

I built a specific configuration for `unlike-assets` called [`polyglot`][rc]
that lets me write web content with arbitrary DSLs.

This snippet computes a Sierpinsky Triangle when building this page.
A cool part about that is that I expressed the code only once. My
system knows to display the code for you to read, and then run it.

<script type="text/racket" id="tri">
#lang racket/base

(require racket/format racket/list)
(provide sierpinsky-triangle)

; Derived from https://goessner.net/articles/svg/fractals/index.html
(define (sierpinsky-triangle iterations color id-prefix)
  (define (gid n) (format "~a~e" id-prefix n))
  (define (rf n) (format "#~a" (gid n)))
  (define (iter n)
    `(g ((id ,(gid n)))
        ,@(map
            (λ (matrix)
               `(use ((xlink:href ,(rf (- n 1))) (transform ,matrix))))
            '("matrix(0.5 0 0 0.5 0 0)"
              "matrix(0.5 0 0 0.5 1 0)"
              "matrix(0.5 0 0 0.5 0.5 0.866)"))))

  `(svg ((xmlns "http://www.w3.org/2000/svg")
         (xmlns:xlink "http://www.w3.org/1999/xlink")
         (style "display: block; margin: 0 auto")
         (width "200")
         (height "175"))
        (defs
          (path ((id ,(gid 0)) (fill ,color) (d "M0 0,2 0,1 1.732 z"))))
          ,@(map
              iter
              (range 1 (+ iterations 1)))
          (use ((xlink:href ,(rf iterations)) (transform "scale(100)")))))
</script>
<script type="application/rackdown">
#lang racket/base
(require "project/vcomps.rkt")
(write (rackdown-code-sample "fractal-example"
    "#lang racket"
    "(require \"tri.rkt\")"
    "(write `(div ((style ,(string-join "
    "                       '(\"margin: 0 auto\")"
    "                       \";\")))"
    "             ,(sierpinsky-triangle 4 \"#800\" \"tri\")))"))
</script>

This page starts as a Markdown file. Since Markdown supports use of HTML tags,
I use `<script>` elements to express Racket modules that are available only
within a page.

* `text/racket` modules act as libraries.
* `application/rackdown` elements write content and define page layout.

I parse the Markdown, process the Racket code in a sensible order, and then
scan for dependencies in `href` and `src` attributes (such as other pages) to
automagically build the rest of the website until all dependencies are fulfilled.

Things get fun when I can "drop down" from prose at any time to prepare
a deeply integrated application that sits snug in any surrounding context.
So if you are interested in using this kind of engine for your code, give
[`polyglot`][rc] a try. If you want to start from a lower level, then use [`unlike-assets`][ua].

[rash]: https://docs.racket-lang.org/rash/index.html
[racket]: https://racket-lang.org/
[ua]: https://github.com/zyrolasting/unlike-assets
[rc]: https://github.com/zyrolasting/polyglot 
