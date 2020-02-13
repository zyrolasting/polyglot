#lang racket/base

(require racket/file
         racket/function
         rackunit
         polyglot/functional
         polyglot/txexpr
         polyglot/paths)

(require/expose polyglot/functional
                (group-scripts!
                 script-info-path
                 script-info-predicate
                 script-info-element))

(test-equal? "Replace element via parameter"
             (parameterize ([current-replace-element-predicate (λ (x) (tag-equal? 's x))])
               (tx-replace-me '(root (rooter (rootest (s))))
                              (λ (x) '((leafy) (greens)))))
             '(root (rooter (rootest (leafy) (greens)))))

(test-case "Can group scripts (with file I/O)"
  (define tx '(root (script ((type "a") (id "q")) "A1")
                    (script ((type "b") (id "r")) "B1")
                    (script ((type "a") (id "s")) "A2")
                    (script ((type "c") (id "t")) "C1")))

  (define tmpd (make-temp-ephmod-directory))
  (define (group! type)
    (group-scripts! tx
                    tmpd
                    (λ (x) (and (list? x)
                                (equal? (attr-ref x 'type #f)
                                        type)))))

  (define a-scripts (group! "a"))
  (define b-scripts (group! "b"))

  (define combined (append a-scripts b-scripts))
  (check-equal? (length a-scripts) 2)
  (check-equal? (length b-scripts) 1)
  (check-true (andmap file-exists?
                      (map script-info-path
                           (append a-scripts
                                   b-scripts))))
  (check-true (andmap (λ (si)
                        ((script-info-predicate si)
                         (script-info-element si)))
                      combined))
  (delete-directory/files tmpd))
