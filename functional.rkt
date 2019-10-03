#lang racket/base

(require racket/contract)
(provide polyglot/functional%
         (contract-out
          [run-txexpr/functional! (-> (or/c (non-empty-listof txexpr?))
                                      (non-empty-listof txexpr?))]))

(require 
  racket/class
  racket/dict
  racket/file
  racket/function
  racket/list
  racket/path
  racket/rerequire
  racket/string
  unlike-assets
  unlike-assets/logging
  unlike-assets/policy
  [except-in markdown xexpr->string]
  "./private/default-file-handling.rkt"
  "./private/dependencies.rkt"
  "./private/dynamic-modules.rkt"
  "./private/fs.rkt"
  "./private/scripts.rkt"
  "./private/writer.rkt"
  "./paths.rkt"
  "./txexpr.rkt")

(module+ test
  (require racket/file
           racket/string
           rackunit))

(define fallback-provided-name 'replace-element)

(define (default-layout kids)
  `(html (head (title "Untitled")) (body . ,kids)))

(define (apply-rackdown tmp-rel elements [initial-layout default-layout])
  (define layout initial-layout)
  (define expanded
    (interlace-txexprs
     elements
     app-script?
     (λ (x)
       (define path (write-script x tmp-rel))
       (define-values (fragment errors) (load-script path))
       (<info "<script> ~a yields fragment:" path)
       (<info "~e" fragment)
       (set! layout (dynamic-require path 'layout (thunk layout)))
       (delete-file path)
       (for ([err errors]) (<error err))
       fragment)))
  (layout expanded))

;; Declare all script nodes expressing inline Racket modules.
(define (with-libraries elements proc)
  (define tmp-rel (make-temp-ephmod-directory))
  (dynamic-wind
    void
    (λ ()
      (proc
        tmp-rel
        (interlace-txexprs
            elements
            lib-script?
            (λ (x)
               (write-script x tmp-rel)
               null))))
    (λ ()
       (delete-directory/files (tmp-rel)))))

(define (run-rackdown elements [initial-layout default-layout])
  (define expanded
    (with-libraries elements (λ (tmp-rel elements)
                               (apply-rackdown tmp-rel
                                               elements
                                               initial-layout))))
  (car (interlace-txexprs
        expanded
        (λ (x) (and (tag-equal? 'p x)
                    (= (length (get-elements x)) 0)))
        (λ _ null))))

(module+ test
  (require rackunit markdown)

  (define elements
    '((p (script ((id "boo") (type "text/racket"))
                 "#lang racket/base"
                 "(provide foo)"
                 "(define foo 1)"))
      (p "The result is")
      (script ((type "application/rackdown"))
         "#lang racket"
         "(require \"./boo.rkt\")"
         "(provide layout)"
         "(write `(span ,(number->string (+ foo 1))))"
         "(define layout (λ (kids) `(body . ,kids)))")))

  (test-equal? "Rackdown"
    (with-handlers ([exn? displayln]) (run-rackdown elements))
    '(body (p "The result is") (span "2"))))


(define polyglot/functional%
  (class* unlike-compiler% () (super-new)
      (define/override (delegate path)
        (case (path-get-extension path)
          [(#".md") markdown->dependent-xexpr]
          [else copy-hashed]))

      (define/override (clarify unclear)
        (define path (build-complete-simple-path unclear (assets-rel)))
        (unless (file-readable? path) (error (format "Cannot read ~a" unclear)))
        path)))


(define (run-txexpr/functional! tx-expressions [initial-layout (λ (kids) kids)])
  (run-rackdown tx-expressions initial-layout))

(define (markdown->dependent-xexpr clear compiler)
  (define txexpr/parsed (parse-markdown clear))
  (define txexpr/preprocessed (send compiler preprocess-txexprs txexpr/parsed))
  (define txexpr/expanded (run-txexpr/functional! txexpr/preprocessed default-layout))
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
