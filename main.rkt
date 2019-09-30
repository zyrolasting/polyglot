#lang racket/base

(require racket/contract)
(provide polyglot%
         discover-dependencies
         apply-manifest
         path-el/c
         polyglot-project-directory
         polyglot-temp-directory
         polyglot-rel
         project-rel
         assets-rel
         dist-rel
         system-temp-rel
         (contract-out
          [run-txexpr! (-> (or/c (non-empty-listof txexpr?) txexpr?)
                           (non-empty-listof txexpr?))]))

(require
  racket/class
  racket/path
  racket/rerequire
  racket/string
  unlike-assets
  unlike-assets/logging
  unlike-assets/policy
  "./private/txexpr.rkt"
  "./private/fs.rkt"
  "./private/paths.rkt"
  "./private/racket-as-asset.rkt"
  "./private/default-file-handling.rkt"
  "./private/rkdown/dependencies.rkt"
  "./private/rkdown/compiler.rkt")

(define fallback-provided-name 'replace-element)

(define polyglot%
  (class* unlike-compiler% () (super-new)
      (define/override (delegate path)
        (case (path-get-extension path)
          [(#".md") markdown->dependent-xexpr]
          [(#".rkt") delegate-to-asset-module]
          [else copy-hashed]))

      (define/override (clarify unclear)
        (define path (build-complete-simple-path unclear (assets-rel)))
        (unless (file-readable? path) (error (format "Cannot read ~a" unclear)))
        path)

      (define/public (preprocess-txexprs tx-expressions)
        (expand-forest
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
  (require rackunit
           racket/file)
  (test-case
    "Can preprocess tagged X-expressions from asset folder"
    (define provided-name "replacer")

    (define (e2e provided-name)
      (define compiler (new polyglot%))
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
