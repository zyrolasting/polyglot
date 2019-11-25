#lang scribble/manual

@title{Why @italic{Another} Web Development... Thing?!}

I designed Polyglot after many years of switching platforms,
frameworks, or libraries to develop websites. Each switch made me
re-learn basic things, all the way back to writing a web page. I can't
make a new tool without being a part of that problem, so why should
you use anything I make in an already saturated space?

For one thing, if you want to change how you work on a website when
neck deep with Polyglot, you can do so without rewriting all of your
code. You can also use any @litchar{#lang} you want to express content
for in-place expansion of a web page. In Racket you get one
@litchar{#lang} per source file to work against an assumed
workflow. Polyglot multiplies this effect by letting you write entire
Racket modules of any @litchar{#lang} @italic{within} one source file,
among prose. How those languages expand as content in a page is up to
your workflow.

Polyglot is not for everybody. It gives you freedom, which means more
work. It won't give you staple featues you think every website should
have. Its core job is to give you flexibility and
backwards-compatibility when you inevitably end up with code that you
for some reason wish would like to leave behind.

Naturally I want you to use Polyglot, but I prefer you use what fits
the job. So unless you identify everything I said up until now, then
you might prefer another tool. If you want something that protects
what you already made while keeping you ready to prototype anything
new, then consider trying Polyglot.