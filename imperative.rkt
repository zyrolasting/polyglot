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
                                        (non-empty-listof txexpr?))])))

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

(module+ test
  (require racket/file
           racket/string
           rackunit))

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

(module+ test
  (require racket/file)
  (test-case
    "Can preprocess tagged X-expressions from asset folder"
    (define provided-name "replacer")
    (define (empty-directory path)
      (delete-directory/files path #:must-exist? #f)
      (make-directory path))

    (define (e2e provided-name)
      (define compiler (new polyglot/imperative%))
      (define module-name (format "macro-~a" provided-name))
      (define tmpdir (system-temp-rel "pg-test"))

      (empty-directory tmpdir)
      (parameterize ([polyglot-project-directory tmpdir])
        (empty-directory (assets-rel))
        (define module-path (assets-rel (format "~a.rkt" module-name)))

        (display-lines-to-file
         `("#lang racket/base"
           "(require txexpr)"
           ,(format "(provide ~a)" provided-name)
           ,(format "(define (~a other)" provided-name)
           "  `((p ,(attr-ref other 'data-foo))))")
         module-path)

        ; We don't want to specify the compiler's fallback name in the input code.
        ; How else would we know if the compiler used the fallback correc
        (define input-code
          `((meta ((itemprop "build")
                   (data-foo "made it")
                   (data-macro ,(format "~a ~a"
                                       module-name
                                       (if (equal? provided-name fallback-provided-name)
                                           ""
                                           provided-name)))))))

        (define changed
          (car (send compiler
                     preprocess-txexprs
                     input-code)))

        (check-true (and (txexpr? changed)
                         (equal? (get-tag changed) 'p)
                         (equal? (car (get-elements changed))
                                 "made it")))))

    (e2e "different-name")
    (e2e fallback-provided-name)))


(define (run-txexpr/imperative! tx-expressions [initial-layout (λ (kids) kids)])
  (run-rackdown tx-expressions initial-layout))

(define (markdown->dependent-xexpr clear compiler)
  (define txexpr/parsed (parse-markdown clear))
  (define txexpr/preprocessed (send compiler preprocess-txexprs txexpr/parsed))
  (define txexpr/processed (run-txexpr/imperative! txexpr/preprocessed))
  (add-dependencies! clear compiler txexpr/processed))

(module+ test
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
        (run-txexpr/imperative! (parse-markdown rackdown)
                                (λ (kids)
                                  `(body . ,kids)))
        '(body (p "Hello") (p "test!")))))
