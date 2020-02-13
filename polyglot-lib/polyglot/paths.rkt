#lang racket/base

(require racket/contract
         racket/runtime-path
         racket/file
         racket/path
         unlike-assets)

(define path-el/c (and/c (or/c path-for-some-system?
                               path-string?)
                         (not/c complete-path?)))
(define path-builder/c (->* () #:rest (listof path-el/c) complete-path?))
(provide path-el/c
         (contract-out
          [path-rel (->* (complete-path?) #:rest (listof path-for-some-system?) path-builder/c)]
          [polyglot-project-directory (parameter/c (and/c complete-path? directory-exists?))]
          [polyglot-temp-directory (parameter/c (and/c complete-path? directory-exists?))]
          [polyglot-assets-directory (parameter/c (or/c #f (and/c complete-path? directory-exists?)))]
          [polyglot-dist-directory (parameter/c (or/c #f complete-path?))]
          [project-rel path-builder/c]
          [assets-rel path-builder/c]
          [dist-rel path-builder/c]
          [polyglot-rel path-builder/c]
          [system-temp-rel path-builder/c]
          [make-temp-ephmod-directory (-> path?)]
          [make-dist-path-string (->* (complete-path?)
                                      (complete-path?)
                                      path-string?)]))

(define-runtime-path polyglot-runtime-path ".")

(module+ test
  ; We'll need paths to private skeleton files to make sure
  ; the test package can build them.
  (provide skels skel-paths)
  (require racket/list)

  (define-runtime-path skels "./private/skel")

  (define skel-paths
    (filter-map
     (λ (name)
       (define path (build-path skels name))
       (and (directory-exists? path)
            ; The extra "." provides a trailing directory separator
            ; we'll need for assertions.
            (simplify-path (build-path path "."))))
     (directory-list skels))))


(define (derive-path-parameter build-conventional-path)
  (make-derived-parameter (make-parameter #f)
                          (λ (v) v)
                          (λ (v)
                            (or v
                                (build-conventional-path)))))

; Use to bind path building functions
(define (path-rel . head)
  (λ tail (simplify-path
             (cleanse-path (apply build-path (append head tail)))
             #f)))

(define (path-builder base-rel next)
  (λ rest (apply (path-rel (base-rel) next)
                 rest)))

(define (make-dist-path-string complete-path [relative-to (dist-rel)])
  (define rel/simple (simple-form-path complete-path))
  (define base/simple (simple-form-path relative-to))
  (if (equal? base/simple rel/simple)
      "/"
      (path->string (find-relative-path base/simple rel/simple))))

(define (make-temp-ephmod-directory)
  (define temp-dir (make-temporary-file "ephmod~a" 'directory (system-temp-rel)))
  (make-file-or-directory-link (project-rel)
                               (build-path temp-dir "project"))
  temp-dir)

(define polyglot-project-directory (make-parameter (current-directory)))
(define polyglot-assets-directory (derive-path-parameter (λ _ (build-path (polyglot-project-directory) "assets"))))
(define polyglot-dist-directory (derive-path-parameter (λ _ (build-path (polyglot-project-directory) "dist"))))
(define polyglot-temp-directory (make-parameter (find-system-path 'temp-dir)))

(define project-rel     (path-builder polyglot-project-directory "."))
(define assets-rel      (path-builder polyglot-assets-directory "."))
(define dist-rel        (path-builder polyglot-dist-directory "."))
(define system-temp-rel (path-builder polyglot-temp-directory "."))
(define polyglot-rel    (path-rel polyglot-runtime-path))
