#lang racket/base

(require racket/class
         racket/file
         racket/path
         unlike-assets/logging)
(provide (all-defined-out))

(define (unclear-asset-path->dist-path compiler unclear-asset-path)
  (send compiler
        lookup
        (send compiler
              clarify
              unclear-asset-path)))

(define (find-dist-relative-path dependent-dist-path dependency-dist-path)
  (let-values ([(base name _) (split-path dependent-dist-path)])
    (path->string (find-relative-path base
                                      dependency-dist-path))))

(define (find-dist-relative-path/unclear compiler dist-resource-path unclear-asset-path)
  (find-dist-relative-path dist-resource-path
                           (unclear-asset-path->dist-path compiler
                                                          unclear-asset-path)))

(define (with-output-to-dist-file path proc)
  (make-parent-directory* path)
  (with-output-to-file path #:exists 'replace proc)
  (<info "Wrote ~a" path)
  path)
