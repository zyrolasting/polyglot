#lang racket/base

(provide (all-defined-out))
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

(define (rackdown->txexpr! path-or-string [initial-layout (λ (kids) kids)])
  (run-rackdown (parse-markdown path-or-string) initial-layout))

(define (markdown->dependent-xexpr clear compiler)
  (define txexpr/expanded (rackdown->txexpr! clear))
  (define unclear-dependencies (discover-dependencies txexpr/expanded))

  (define next
    (chain build-page
         txexpr/expanded
         unclear-dependencies
         clear
         compiler))

  (define (ripple clear dep/clear history)
    (if (equal? (path-get-extension clear) #".css")
        next
        (first history)))

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

(module+ test
  (require racket/file
           racket/string
           rackunit)
  (define tmp (make-temporary-file))
  (delete-file tmp)
  (make-directory tmp)
  (parameterize ([current-directory tmp]
                 [polyglot-project-directory tmp])
    (display-lines-to-file
     '("#lang racket/base"
       "(provide datum)"
       "(define datum '(p \"test!\"))")
     "datum.rkt")

    (define rackdown
      (string-join
       '("Hello"
        "<script type=\"text/racket\" id=\"lib\">"
        "#lang racket/base"
        "(require \"project/datum.rkt\")"
        "(provide from-lib)"
        "(define from-lib datum)"
        "</script>"
        "<script type=\"application/rackdown\">"
        "#lang racket/base"
        "(require \"lib.rkt\")"
        "(write from-lib)"
        "</script>")
       "\n"))

    (test-equal?
        "Process Rackdown externally"
        (rackdown->txexpr! rackdown
                           (λ (kids)
                             `(body . ,kids)))
        '(body (p "Hello") (p "test!")))))
