#lang racket/base

(require racket/file
         "../main.rkt"
         "./cli/shared.rkt"
         "./fs.rkt")

(define (fresh-test-project skel)
  (define test-project-path (make-temporary-file "polyglot-test-~a"
                                                 'directory
                                                 (system-temp-rel)))
  (when (directory-exists? test-project-path)
    (delete-directory/files test-project-path))
  (copy-skeleton skel test-project-path))

(module+ test
  (require racket/class
           rackunit)

  (for ([workflow '("functional" "imperative")]
        [cls (list polyglot/functional% polyglot/imperative%)])
    (test-case
      (format "Can build test project (~a)" workflow)
      (define test-project-path (fresh-test-project workflow))
      (parameterize ([polyglot-project-directory test-project-path])
        (define compiler (make-compiler))
        (clear-distribution!)
        (send compiler add! (assets-rel "index.md"))
        (send compiler compile!)
        (check-equal? (length (directory-list (assets-rel)))
                      (length (directory-list (dist-rel))))))))
