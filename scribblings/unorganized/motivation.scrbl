#lang scribble/manual

@title[#:tag "motivation"]{Why @italic{Another} Web Development... Thing?!}

I designed Polyglot to address my own common pains after many years of
switching platforms, frameworks, or libraries to develop
websites. Each switch often made me learn the next trendy rephrasal of
basic concepts and tasks. I can't make a new tool without being a part
of that problem, so why should you use anything I make in an already
saturated space?

For one thing, you control the subjective parts that are normally
decided for you. If you want to change how you work on a website when
neck deep with Polyglot, you can do so without rewriting all of your
code. It decouples what code is from what code means for your
purposes.

Unlike Scribble and Pollen, Polyglot views content as free-form
documents that @italic{contain} programs of any language, as opposed
to programs themselves. When documents are programs, you have no
control over the inner representation of the document except through
an API for a prescribed workflow. Polyglot allows you to control the
workflow itself. This gives you flexibility whenever you want to
change how you work, at the cost of maintaining workflow-specific code
to reproduce your website.

Polyglot comes with conveniences such as Markdown posts,
cache-busting, dependency processing, and an S3 upload tool. All
behaviors are overridable so that you can do away with any opinionated
bits while keeping the utilities that handle the hard parts. You can
use a built-in workflow to start a limited static blog within
seconds. Once you are ready, you can modify the workflow to handle
interactive technical portfolios, monitoring dashboards, and other
deeply-integrated web applications.

Polyglot is not for everybody. It gives you freedom, which means
work. It won't give you staple featues you think every website should
have. Its core job is to give you flexibility and
backwards-compatibility when you inevitably end up with code that you
want to use, yet leave behind.

Naturally I want you to use Polyglot, but I prefer you use what fits
the job. So unless you identify with everything I said up until now,
then you might prefer another tool. If you want something that
protects what you already made while keeping you ready to prototype
anything new, then consider trying Polyglot.