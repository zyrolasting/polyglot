#lang racket/base

(provide skel-paths)
(require racket/class
         racket/file
         racket/list
         rackunit
         unlike-assets
         polyglot/projects
         polyglot/paths
         (submod polyglot/paths test))

(test-true "There are at least two skeleton projects"
           (>= (length skel-paths) 2))

(test-case "useable-polyglot-directory?"
  (for ([path skel-paths])
    (check-true (useable-polyglot-directory? path)))
  (check-false (useable-polyglot-directory? skels)))

(test-case "find-closest-project"
  (define get-directory (class-field-accessor polyglot-project% directory))
  (for ([fn skel-paths])
    (parameterize ([polyglot-project-directory fn])
      (define obj (find-closest-project (assets-rel "index.md")))
      (check-true (is-a? obj polyglot-project%))
      (check-equal? fn (get-directory obj)))))

(test-case "copy-polyglot-skeleton-project!"
  (test-case "can create useable projects as a result of the copy"
    (for ([path skel-paths])
      (define-values (base name _) (split-path path))
      (define project
        (copy-polyglot-skeleton-project! #:force? #t
                                         (path->string name)
                                         (make-temporary-file (format "~a~~a" name)
                                                              'directory)))
      (define dir (get-field directory project))
      (check-pred useable-polyglot-directory? dir)
      (delete-directory/files dir)))
  (test-case "will raise exn:fail:filesystem on existing directory when #:force? is #f"
    (define dir (make-temporary-file "polyglot~a" 'directory))
    (check-exn exn:fail:filesystem?
               (λ _
                 (copy-polyglot-skeleton-project! #:force? #t "functional" dir)
                 (copy-polyglot-skeleton-project! #:force? #f "functional" dir)))
    (delete-directory/files dir)))


(test-case "polyglot-project%"
  (define dirA (car skel-paths))
  (define dirB (cadr skel-paths))

  (test-equal? "polyglot-project% instances are equal? if their directories are equal?"
               (new polyglot-project% [directory dirA])
               (new polyglot-project% [directory (build-path dirA ".")]))

  (define (class-provided? path)
    (subclass? (send (new polyglot-project% [directory path]) get-workflow-class (λ _ #f))
               unlike-compiler%))

  (test-true "Skeleton rcfiles load without issue"
             (and (class-provided? dirA)
                  (class-provided? dirB)))

  (test-case "ensure-empty-distribution!"
    (define obj (new polyglot-project% [directory dirA]))
    (parameterize ([polyglot-project-directory dirA])
      (make-directory* (dist-rel))
      (display-to-file #:exists 'replace "" (dist-rel "blah"))
      (test-true "Dist directory has a file"
                 (> (length (directory-list (dist-rel))) 0))
      (send obj ensure-empty-distribution!)
      (test-true "Dist directory exists" (directory-exists? (dist-rel)))
      (test-true "Dist directory is empty" (= 0 (length (directory-list (dist-rel))))))))
