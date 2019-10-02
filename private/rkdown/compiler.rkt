#lang racket/base

(provide (all-defined-out))
(require
  racket/list
  racket/class
  racket/dict
  racket/path
  racket/string
  unlike-assets
  unlike-assets/logging
  unlike-assets/policy
  [except-in markdown xexpr->string]
  "../../txexpr.rkt"
  "../paths.rkt"
  "../fs.rkt"
  "./dependencies.rkt"
  "./evaluator.rkt")

(module+ test
  (require rackunit))


(define (run-txexpr! tx-expressions [initial-layout (λ (kids) kids)])
  (run-rackdown
   (if (txexpr? tx-expressions)
       (list tx-expressions)
       tx-expressions)
   initial-layout))

(define (markdown->dependent-xexpr clear compiler)
  (define txexpr/parsed (parse-markdown clear))
  (define txexpr/preprocessed (send compiler preprocess-txexprs txexpr/parsed))
  (define txexpr/expanded (run-txexpr! txexpr/preprocessed default-layout))
  (define unclear-dependencies (discover-dependencies txexpr/expanded))

  (define next
    (chain build-page
         txexpr/expanded
         unclear-dependencies
         clear
         compiler))

  (define (ripple . _) next)

  (for ([unclear unclear-dependencies])
    (define dep/clear (send compiler clarify unclear))
    (if (equal? (path-get-extension dep/clear) #".md")
        (send compiler
              add!
              dep/clear)
        (send compiler
              add!
              dep/clear
              clear
              ripple)))

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

(module+ test
  (test-case
    "Output does not change when normalizing arguments"
    (define node '(p () "a"))
    (check-equal? (run-txexpr! node)
                  (run-txexpr! `(,node)))))

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
        (run-txexpr! (parse-markdown rackdown)
                     (λ (kids)
                       `(body . ,kids)))
        '(body (p "Hello") (p "test!")))))
