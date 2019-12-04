#lang scribble/manual
@require[@for-label[aws/keys
                    polyglot
                    racket/base]]

@title[#:tag "setup"]{Get Started}

To confirm that everything works, install the package and run the @tt{demo}
command.

@verbatim[#:indent 2]|{
$ raco pkg install polyglot
$ polyglot demo
}|

If the command succeeds, then you will see a @tt{dist} folder appear
in your working directory containing a @tt{README.html} file. This is
a @italic{distribution} built from the
@hyperlink["https://github.com/zyrolasting/polyglot/blob/master/README.md"
"README in the source code"].

If the command fails, Polyglot is probably unable to create symbolic links. It
needs symbolic to function (See @racket[make-file-or-directory-link]). On
current versions of Windows, you may need to give an account permission to
create links. If it's just not working and you need help,
@hyperlink["https://github.com/zyrolasting/polyglot/issues"]{open an issue}.

@section{Step 1: Start Your Project}

Run this command to start a new project.

@verbatim[#:indent 2]|{
$ polyglot start my-website
}|

You'll see a new directory called @tt{my-website}. It has some starter code
to play with.

@section{Step 2: Write Content}

The @tt{develop} command rebuilds your website in response to changes in
assets. Let it run in another terminal.

@verbatim[#:indent 2]|{
$ polyglot develop my-website
}|

Open up one of the Markdown files and make some changes. Break the code in it,
and watch what happens. While the @tt{develop} command is active, it will
build only the assets relevant to your change. The distribution directory
will update with new files as you go.

Building the website once is just as easy, if you need to spot-check something.

@verbatim[#:indent 2]|{
$ polyglot build my-website
}|


@section{Step 3: Publish}
Once you're ready, just copy your distribution directory. If it helps,
@tt{polyglot} includes a command to rebuild your website and push the
distribution to an S3 bucket.

@verbatim[#:indent 2]|{
$ polyglot publish --region us-east-2 my-website my-bucket
}|

For more details on what @italic{exactly} this does, see
@secref["polyglot_publish__Publish_to_S3" #:doc '(lib
"polyglot/scribblings/reference/polyglot-reference.scrbl")]

@section{So What?}

Once again we see that Markdown + CSS = styled HTML. Snore.

Except hold on: If you look at the Markdown, you'll find it has @tt{<script>}
elements containing code from... different Racket languages? You'll also notice
that something is crawling the source code, since there's no @tt{import} or
something pulling in new pages. The output pages actually have layouts, but
there's no templating engine. On top of it all, this isn't even an extended
Markdown. In fact, this isn't even using all of the features of the Daring
Fireball spec.

This is where we start seeing where Polyglot is unique. It can tease out useful
features despite being given so little to work with.

On its own, Polyglot has no idea how to process content. It needs a
@defterm{workflow} to define what assets the website supports, and what happens
to those assets when you build the project. It gives you room to extend
languages arbitrarily to contain cooperating Racket modules, and it just so
happens to use Markdown as a familiar prose language to "host" programs.

You'll notice there's a hidden @tt{.polyglotrc.rkt} file in your project
directory. Let's open it up:

@racketmod[#:file ".polyglotrc.rkt"
racket/base
(require polyglot)
(provide (rename-out [polyglot/imperative% polyglot+%]))
]

@margin-note{Don't worry if the workflow isn't ideal. It's not hard to change it later.}
This module provides a workflow as @racket[polyglot+%], which is just a
class. This configuration tells Polyglot to use @secref{imperative-workflow},
which exists in code as @racket[polyglot/imperative%].

Before we dive deep into the Imperative Workflow, we're going to talk about
what has happened to the Markdown code.