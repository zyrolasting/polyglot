<script type="application/rackdown">
#lang racket/base
(require "project/vcomps.rkt")
(provide layout)
(define layout (λ (kids) (page "I Built This Website Using Racket. Here's What I Can Do Now." kids)))
</script>

_This is an excerpt from the announcement page for the project.
The original article can be found [here](https://sagegerard.com/racket-powered.html)._

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

