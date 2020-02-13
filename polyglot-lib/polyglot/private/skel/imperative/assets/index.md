<script type="application/racket">
#lang racket

;; This is an application element. It can control the page and generate content.

;; 'project' is always a symbolic link to the project directory you give to polyglot.
;; Use it to access shared resources.
(require "project/vcomps.rkt")

;; polyglot will look for a procedure that defines the layout for your page.
;; Only application elements may provide a layout. If multiple elements provide a layout,
;; polyglot will use only the last one received.
(provide layout)

(define layout (λ (kids) (page "Home" kids)))
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
