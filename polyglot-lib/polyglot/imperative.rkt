#lang racket/base

(provide polyglot/imperative%
         (rename-out [polyglot/imperative% polyglot%]
                     [run-txexpr/imperative! run-txexpr!])
         run-txexpr/imperative!)

(module+ safe
  (require racket/contract)
  (provide polyglot/imperative%
           (rename-out [polyglot/imperative% polyglot%]
                       [run-txexpr/imperative! run-txexpr!])
           (contract-out
            [run-txexpr/imperative! (-> (or/c (non-empty-listof txexpr?) txexpr?)
                                        (or/c txexpr? (non-empty-listof txexpr?)))])))

(require racket/class
         racket/dict
         racket/file
         racket/function
         racket/list
         racket/path
         racket/port
         racket/rerequire
         racket/string
         unlike-assets
         unlike-assets/logging
         unlike-assets/policy
         [except-in markdown xexpr->string]
         "./elements.rkt"
         "./base.rkt"
         "./paths.rkt"
         "./txexpr.rkt")


(define fallback-provided-name 'replace-element)
(define default-layout make-minimal-html-page)

(define (apply-rackdown tmpd elements [initial-layout default-layout])
  (define layout initial-layout)
  (define expanded
    (interlace-txexprs
     elements
     app-element?
     (λ (x)
       (define path (write-script x tmpd))
       (define-values (stdout stdin stderr) (load-script path))
       (define-values (fragment errors) (values (port->list read stdout)
                                                (port->list read stderr)))
       (<info "<script> ~a yields fragment:" path)
       (<info "~e" fragment)
       (set! layout (dynamic-require path 'layout (thunk layout)))
       (delete-file path)
       (for ([err errors]) (<error err))
       fragment)))
  (layout expanded))

;; Declare all script nodes expressing inline Racket modules.
(define (with-libraries elements proc)
  (define tmpd (make-temp-ephmod-directory))
  (dynamic-wind
    void
    (λ ()
      (proc
        tmpd
        (interlace-txexprs
            elements
            lib-element?
            (λ (x)
               (write-script x tmpd)
               null))))
    (λ ()
       (delete-directory/files tmpd))))

(define (run-rackdown elements [initial-layout default-layout])
  (define expanded
    (with-libraries elements (λ (tmpd elements)
                               (apply-rackdown tmpd
                                               elements
                                               initial-layout))))
  (define interlaced
    (interlace-txexprs
        expanded
        (λ (x) (and (tag-equal? 'p x)
                    (= (length (get-elements x)) 0)))
        (λ _ null)))

  (if (txexpr? expanded)
      (car interlaced)
      interlaced))


(define polyglot/imperative%
  (class* polyglot/base% () (super-new)
      (define/override (delegate path)
        (case (path-get-extension path)
          [(#".md") markdown->dependent-xexpr]
          [else (super delegate path)]))

      (define/public (preprocess-txexprs tx-expressions)
        (interlace-txexprs
         tx-expressions
         (λ (x)
           (and (txexpr? x)
                (string? (attr-ref x 'data-macro #f))))
         (λ (x)
           (define modspec/list
             (regexp-split #px"\\s+"
                           (string-trim (attr-ref x 'data-macro))))

           (define module-path
             (assets-rel (format "~a.rkt"
                                 (car modspec/list))))

           (define provided
             (if (> (length modspec/list) 1)
                 (string->symbol (cadr modspec/list))
                 fallback-provided-name))

           (<debug "data-macro: load `~a` from `~a`"
                   provided
                   module-path)

           (dynamic-rerequire module-path)
           ((dynamic-require module-path provided) x))))))


(define (run-txexpr/imperative! tx-expressions [initial-layout (λ (kids) kids)])
  (run-rackdown tx-expressions initial-layout))

(define (markdown->dependent-xexpr clear compiler)
  (define txexpr/parsed (parse-markdown clear))
  (define txexpr/preprocessed (send compiler preprocess-txexprs txexpr/parsed))
  (define txexpr/processed (run-txexpr/imperative! txexpr/preprocessed))
  (add-dependencies! clear compiler txexpr/processed))
