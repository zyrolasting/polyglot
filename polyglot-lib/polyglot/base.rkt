#lang racket/base

(require racket/contract)
(provide polyglot/base%
         add-dependencies!
         (contract-out
          [make-minimal-html-page (-> (listof txexpr-element?)
                                      txexpr?)]))


(require racket/class
         racket/dict
         racket/file
         racket/list
         racket/path
         racket/rerequire
         racket/sequence
         racket/string
         file/sha1
         unlike-assets
         unlike-assets/logging
         unlike-assets/policy
         "./txexpr.rkt"
         "./paths.rkt"
         "./private/css.rkt"
         "./private/dist.rkt")

(define (make-minimal-html-page body)
  `(html (head (title "Untitled")) (body . ,body)))

(define/contract (delegate-to-asset-module clear compiler) advance/c
  (<info "Delegating to ~a's write-dist-file" clear)
  (dynamic-rerequire clear)
  (dynamic-require clear 'write-dist-file))


; Translate resolved dependencies and prior X-expression to fulfilled document
(define (resolve-dependencies txexpr/expanded unclear-deps clear compiler)
  (define output-html-path
    (dist-rel
     (path-replace-extension
      (make-dist-path-string clear
                             (assets-rel))
      #".html")))

  (define-values (literal-refs non-literal-refs)
    (partition (λ (s) (equal? (path-get-extension s) #".literal"))
               unclear-deps))

  (define txexpr/literals-replaced
    (apply-manifest txexpr/expanded
                    (make-immutable-hash (map (λ (x) (cons x x))
                                              literal-refs))
                    (λ (to-rewrite)
                      (path->string (path-replace-extension to-rewrite
                                                            #"")))))

  (define txexpr/final
    (apply-manifest txexpr/literals-replaced
                    (make-immutable-hash
                     (map (λ (unclear)
                            ; Accounts for the fact that .md files
                            ; don't have strict dependency
                            ; relationships.
                            (define normalized
                              (if (equal? (path-get-extension unclear) #".md")
                                  (dist-rel (path-replace-extension unclear #".html"))
                                  (unclear-asset-path->dist-path compiler unclear)))

                            (cons unclear normalized))
                          non-literal-refs))
                    (λ (to-rewrite)
                      (find-dist-relative-path output-html-path
                                               to-rewrite))))

  (with-output-to-dist-file output-html-path
    (λ _ (displayln "<!DOCTYPE html>")
         (displayln (xexpr->html txexpr/final)))))


(define (add-dependencies! clear compiler txexpr/expanded)
  (define unclear-dependencies (discover-dependencies txexpr/expanded))
  (define advance
    (chain resolve-dependencies
           txexpr/expanded
           unclear-dependencies
           clear
           compiler))

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
              (λ _ advance))))
  advance)


;; Copies a file with a basename equal to the
;; first 8 characters of the SHA-1 content hash.
(define (copy-hashed src compiler)
  (define dst
    (call-with-input-file
      src
      (λ (port)
        (dist-rel
         (path-replace-extension
          (substring (sha1 port) 0 8)
          (path-get-extension src))))))

  (unless (file-exists? dst)
    (copy-file src dst))
  dst)

(define (preserve-path clear compiler)
  clear)

(define polyglot/base%
  (class* unlike-compiler% () (super-new)
    (define/override (delegate path)
      (case (path-get-extension path)
        [(#".literal") preserve-path]
        [(#".css") process-css]
        [(#".rkt") delegate-to-asset-module]
        [else copy-hashed]))

    (define/override (clarify unclear)
      (define path (resolve-path (build-complete-simple-path unclear (assets-rel))))
      (unless (or (equal? (path-get-extension path) #".literal")
                  (file-readable? path))
        (error (format "Cannot read ~a" unclear)))
      path)))
