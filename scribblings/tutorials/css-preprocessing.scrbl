#lang scribble/manual

@require[@for-label[racket polyglot css-expr] "../macros.rkt"]

@title[#:tag "upgrade-css"]{Tutorial: Preprocessing CSS}

@(base-workflow) only does some dependency work with CSS, but does
not offer minification or preprocessing. As it turns out, we
can get both using @racketmodname[css-expr]. This tutorial
will show you a way to integrate @racket[css-expr] into your workflow.


@section{Step 1: Start a New Unstyled Project}

Run @tt{polyglot start my-project}. A @tt{my-project} directory will appear
with some starter code. You'll notice that when you build this project using
@tt{polyglot build}, there are already some styles.

@section{Step 2: Get Rid of the Starter Stylesheet}

Delete the @tt{my-project/assets/styles.css} file to remove all styles
from the site. If you visit the page, you'll see that it looks uglier.

@section{Step 3: Install css-expr}

Run @tt{raco pkg install css-expr}, or install the same package using
DrRacket. You'll need this to write styles in the next step.

@section{Step 4: Write a Style Module}

Write the following into a @tt{styles.rkt} module in your assets directory. If
it is inside the assets directory, @tt{polyglot develop} will monitor changes
to it during a live build.

@racketmod[#:file "my-project/assets/styles.rkt"
racket/base

(require css-expr)
(provide css)

(define spacing '20px)
(define fg-color '|#222|)

(define site-styles (css-expr
[* #:box-sizing border-box]

[body
  #:margin (,spacing auto)
  #:padding 0 ,spacing
  #:max-width 50ch
  #:font-size 20px
  #:line-height 1.3]

[.code-sample
 [pre
    #:margin-bottom 0]
 [output
  #:display block
  #:padding 1rem
  #:border (1px ,fg-color solid)]]))

(define css (css-expr->css site-styles))]

The styles shown here are actually the same as the styles in the starter
website. This module simply provides a @tt{css} identifier bound to CSS code as
a Racket string. You'll notice that you can use variables and nested selectors,
just like you could with other CSS preprocessors.

Fire up DrRacket or the REPL. You should see that this session works.

@racketinput[(require "styles.rkt")]
@racketinput[css]
@racketresult["*{box-sizing:border-box;}body{margin:20px auto;padding:0,20px;max-width:50ch;font-size:20px;line-height:1.3;}.code-sample pre{margin-bottom:0;}.code-sample output{display:block;padding:1rem;border:1px #222 solid;}"]

Nice! Save for the extra semicolons at the end of each ruleset, the CSS is
minified.

@section{Step 5: Add to the Workflow}

Open your workflow module @tt{.polyglotrc.rkt} and add this
code to make your project generate a new @tt{styles.css}
on every build.

@racketmod[#:file "my-project/.polyglotrc.rkt"
racket/base
(require racket/runtime-path
         racket/rerequire
         polyglot)

(provide polyglot+%)
(define-runtime-path styles-module "./assets/styles.rkt")

(define (get-styles)
  (and (polyglot-live?)
       (> (length (dynamic-rerequire styles-module)) 0)
       (dynamic-require styles-module 'css)))

(define (generate-styles!)
  (define styles (get-styles))
  (when styles
    (display-to-file #:exists 'replace
      styles
      (assets-rel "styles.css"))))

(define polyglot+%
  (class polyglot/functional%
    (define/override (compile! #:changed c #:removed r)
      (generate-styles!)
      (super compile! #:changed c #:removed r))
    (super-new)))]

This is a situation where instead of writing a file to a distribution,
you are @italic{generating an asset} before a build starts.

This example is slightly crude because the stylesheet is rebuilt from scratch
every time. That's okay for small stylesheets.

But what's going on with @racket[get-styles]? There seems to be more going on
here.

@racketblock[
(define (get-styles)
  (and (polyglot-live?)
       (> (length (dynamic-rerequire styles-module)) 0)
       (dynamic-require styles-module 'css)))]

Remember that Polyglot supports live builds using @tt{polyglot develop}.
It would not be fun to change your styles in Racket and manually
start a new build every time you want to see a change.

@racket[(polyglot-live?)] tells us if we're in an ongoing build. If we are, we
use @racket[dynamic-rerequire] to reload the styles module if it has
changed. We check the return value of @racket[dynamic-rerequire] in
@racket[generate-styles!] to see if a load or reload happened at all. If it
did, we write the stylesheet.

@section{Wrapping Up}

This tutorial took a starter project and upgraded the workflow to use
@racketmodname[css-expr]. This lets you write CSS using variables, nested
selectors, and the full power of Racket. This also opens the door to sharing
theme information with other code like JavaScript and SVG markup.

If your styles start getting bloated, you can always split them up
so that @tt{styles.rkt} requires rules from other modules
and combines them together.

@racketmod[#:file "my-project/assets/styles.rkt"
racket/base
(require css-expr "./styles.d/typography.rkt" "./styles.d/theme.rkt")
(provide css)
(define css (css-expr->css (css-expr ,typography ,theme)))]


@section{Troubleshooting Notes}

@subsection{@tt{"styles.rkt does not exist"}} Emacs users may encounter an
error stating that @tt{styles.rkt} does not exist during a live build. This is
because some editors might temporarily delete a file before renaming an
autosaved version to take its place. Polyglot will detect the deletion both the
deletion and the re-addition of the file as changes that warrant a new
build. Try refreshing your browser after making a style change to see
if your new style rules apply despite the error. If they did, you can
safely ignore it.

If you want to do away with the error entirely, try specifying the
@tt{--delay} option in @tt{polyglot develop} to aggregate more
filesystem changes before trying a new build.

@verbatim|{$ polyglot develop --delay 1000 my-project}|
