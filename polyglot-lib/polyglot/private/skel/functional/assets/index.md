<script type="application/racket" id="home-main">
#lang racket/base

;; This is an application element. It can define a new page.
(provide replace-page)

;; Use the 'project' link to access your project.
(require "project/vcomps.rkt"
         polyglot)

(define (replace-page page-tx)
  (page "Home" page-tx))
</script>

This is your home page. `polyglot` sees `assets/index.md` as the entry point to your website.

## Pages

These are just links to other pages. You can reference then as links to Markdown files,
and `polyglot` will process them in turn. This helps prevent broken links.

* [A Website With Superpowers](racket-powered.md)
* [Dummy contact form](contact.md)

## Meta

* [`polyglot` documentation][docs]
* [Help pay for development!][pay]

[pay]: https://www.paypal.com/paypalme2/sagegerard
[docs]: https://docs.racket-lang.org/polyglot/index.html
