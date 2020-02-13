#lang racket/base

(require racket/file
         rackunit
         polyglot/paths
         (submod polyglot/paths test))

(test-case "make-dist-path-string"
  (test-equal? "Produce relative paths as strings given two complete paths"
               (make-dist-path-string (build-path "/blah" "some/where.rkt")
                                      (build-path "/blah"))
               "some/where.rkt")
  (test-equal? "Return webroot path if paths are the same"
               (make-dist-path-string (build-path "/blah/")
                                      (simplify-path (build-path "/blah/" 'same)))
               "/"))

(test-case "Users may override derived paths"
  (define skel-path (car skel-paths))
  (parameterize ([polyglot-project-directory skel-path])
    (define (check-fallbacks)
      (check-equal? (polyglot-assets-directory) (build-path (polyglot-project-directory) "assets"))
      (check-equal? (polyglot-dist-directory) (build-path (polyglot-project-directory) "dist")))
    (dynamic-wind
      check-fallbacks
      (λ _
        (parameterize ([polyglot-project-directory skel-path]
                       [polyglot-assets-directory skel-path]
                       [polyglot-dist-directory skel-path])
          (check-equal? (polyglot-assets-directory) (polyglot-dist-directory))
          (check-equal? (polyglot-assets-directory) (polyglot-project-directory))))
      check-fallbacks)))
