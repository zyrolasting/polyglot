#lang racket/base

;;; Responsible for dependency references in tagged X-expressions.

(provide (all-defined-out))

(require
  racket/class
  racket/dict
  racket/format
  racket/function
  racket/list
  racket/sequence
  racket/string
  net/url
  unlike-assets
  unlike-assets/policy
  "../txexpr.rkt")

(module+ test
  (require rackunit))

(define (create-manifest unclear-deps compiler)
  (sequence-fold
    (λ (result unclear)
       (dict-set result
                 unclear
                 (send compiler lookup
                       (send compiler clarify unclear))))
    (make-immutable-hash)
    unclear-deps))

(define (get-dependency-ref node)
  (attr-ref node 'src
            (lambda _
              (attr-ref node 'href
                        (lambda _ #f)))))

(define (get-dependency-key x)
  (or (and (attrs-have-key? x 'src) 'src)
      (and (attrs-have-key? x 'href) 'href)
      #f))

(define (discover-dependencies x)
  (map
    get-dependency-ref
    (or
      (findf*-txexpr x
        (lambda (node)
          (and (txexpr? node)
               (let ([ref (get-dependency-ref node)])
                 (and (not (equal? ref "/")) 
                      (local-asset-url? (get-dependency-ref node)))))))
      null)))

(module+ test
  (check-equal?
    (discover-dependencies
      '(html
        (head
          (link ((href "blah.css"))))
        (body ((id "top"))
          (main
            (article
              (section
                (h1 "Heading")
                (a ((href "https://example.com"))
                   (img ((src "mark.png")))))
              (section
                (h1 "Heading")
                (a ((href "/path/to/image.svg")) "Click to view")))
              (a ((href "#top")) "Back to top"))
            (script ((type "text/javascript") (src "main.js"))))))
    '("blah.css"
    "mark.png"
    "/path/to/image.svg"
    "main.js")))


(define (asset-basename path)
  (define-values (base name must-be-dir?)
    (split-path path))
  name)

(define (apply-manifest content manifest)
  (sequence-fold
    (λ (result k v)
      (let-values ([(next _)
                    (splitf-txexpr
                      result
                      (λ (node)
                        (with-handlers ([exn:fail? (thunk* #f)])
                          (equal? (attr-ref node (get-dependency-key node))
                                  k)))
                      (λ (node)
                        (attr-set node
                                  (get-dependency-key node)
                                  (asset-basename v))))])
        next))
    content
    (in-dict manifest)))

(module+ test
  (test-equal?
    "Manifest can replace `src` and `href` attributes"
    (apply-manifest '(html
                       (head
                         (link ((href "a.css"))))
                       (body
                         (img ((src "b.png")))))
                    '(("a.css" . "123.css")
                     ("b.png" . "456.png")))
    '(html
       (head
         (link ((href "123.css"))))
       (body
         (img ((src "456.png")))))))
