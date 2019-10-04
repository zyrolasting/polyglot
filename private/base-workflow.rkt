#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/dict
         racket/file
         racket/path
         racket/sequence
         racket/string
         file/sha1
         unlike-assets
         unlike-assets/policy
         "../txexpr.rkt"
         "../paths.rkt"
         "./fs.rkt")

(define (make-minimal-html-page body)
  `(html (head (title "Untitled")) (body . ,body)))

(define (create-manifest unclear-deps compiler)
  (sequence-fold
    (λ (result unclear)
       (dict-set result
                 unclear
                 (send compiler lookup
                       (send compiler clarify unclear))))
    (make-immutable-hash)
    unclear-deps))


; Translate resolved dependencies and prior X-expression to fulfilled document
(define (resolve-dependencies txexpr/expanded unclear-deps clear compiler)
  (define path (dist-rel (path-replace-extension (basename clear) #".html")))
  (define manifest (create-manifest unclear-deps compiler))

  ; Accounts for the fact we do not mark .md files as dependencies of one another.
  (define manifest/patched
    (for/fold ([modified #hash()])
              ([(k v) (in-dict manifest)])
      (dict-set modified k
                (if (string-suffix? k ".md")
                    (dist-rel (regexp-replace #rx"\\.md$" k ".html"))
                    v))))

  (define txexpr/final (apply-manifest txexpr/expanded
                                       manifest/patched))

  (with-output-to-file path #:exists 'replace
    (λ ()
      (displayln "<!DOCTYPE html>")
      (displayln (xexpr->html txexpr/final))))

  path)


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

(define polyglot/base%
  (class* unlike-compiler% () (super-new)
    (define/override (delegate clear)
      copy-hashed)
    (define/override (clarify unclear)
      (define path (build-complete-simple-path unclear (assets-rel)))
      (unless (file-readable? path) (error (format "Cannot read ~a" unclear)))
      path)))
