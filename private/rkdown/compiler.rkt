#lang racket/base

(provide markdown->dependent-xexpr)
(require
  racket/list
  racket/class
  racket/path
  unlike-assets
  unlike-assets/logging
  unlike-assets/policy
  [except-in markdown xexpr->string]
  "../paths.rkt"
  "../fs.rkt"
  "../txexpr.rkt"
  "./dependencies.rkt"
  "./evaluator.rkt")

(module+ test
  (require rackunit))

(define (markdown->dependent-xexpr clear compiler)
  (define txexpr-fragment/initial (parse-markdown clear))
  (define txexpr/expanded (run-rackdown txexpr-fragment/initial))
  (define unclear-dependencies (discover-dependencies txexpr/expanded))

  (define next
    (chain build-page
         txexpr/expanded
         unclear-dependencies
         clear
         compiler))

  (define (ripple clear dep/clear history)
    (if (equal? (path-get-extension clear) #".md")
        (first history)
        next))

  (for ([unclear unclear-dependencies])
    (send compiler add!
          (send compiler clarify unclear)
          clear
          ripple))

  next)

(define (get-html-path asset)
  (dist-rel (path-replace-extension
             (basename asset)
             #".html")))

(define (write-page path xexpr)
  (with-output-to-file path
    #:exists 'truncate
    (λ ()
      (displayln "<!DOCTYPE html>")
      (displayln (xexpr->html xexpr))))
  (<info "Wrote ~a" path))

; Translate resolved dependencies and prior X-expression to fulfilled document
(define (build-page txexpr/expanded unclear-deps clear compiler)
  (define path (get-html-path clear))
  (define manifest (create-manifest unclear-deps compiler))
  (write-page path (apply-manifest txexpr/expanded manifest))
  path)
