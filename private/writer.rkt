#lang racket/base

(provide (all-defined-out))
(require racket/dict
         racket/path
         racket/string
         unlike-assets
         "../txexpr.rkt"
         "./paths.rkt"
         "./fs.rkt"
         "./dependencies.rkt")

(define (get-html-path asset)
  (dist-rel (path-replace-extension
             (basename asset)
             #".html")))

(define (write-page path xexpr [doctype-line "<!DOCTYPE html>"])
  (with-output-to-file path
    #:exists 'truncate
    (λ ()
      (displayln doctype-line)
      (displayln (xexpr->html xexpr)))))

; Translate resolved dependencies and prior X-expression to fulfilled document
(define (build-page txexpr/expanded unclear-deps clear compiler)
  (define path (get-html-path clear))
  (define manifest (create-manifest unclear-deps compiler))

  ; Account for the fact we do not mark .md files as dependencies of one another.
  (define manifest/patched
    (for/fold ([modified #hash()])
              ([(k v) (in-dict manifest)])
      (dict-set modified k
                (if (string-suffix? k ".md")
                    (dist-rel (regexp-replace #rx"\\.md$" k ".html"))
                    v))))

  (write-page path (apply-manifest txexpr/expanded
                                   manifest/patched))
  path)
