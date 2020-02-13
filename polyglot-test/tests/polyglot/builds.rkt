#lang racket/base

(require racket/class
         racket/file
         racket/list
         rackunit
         unlike-assets
         polyglot/base
         polyglot/builds
         polyglot/paths
         polyglot/projects
         (submod polyglot/paths test))

(require/expose polyglot/builds
                (keep-own-asset-paths))

(test-case "keep-own-asset-paths"
  (define project (copy-polyglot-skeleton-project! #:force? #t "functional"))
  (define other-project (copy-polyglot-skeleton-project! #:force? #t "functional"))
  (define tmpdir (get-field directory project))
  (define other-tmpdir (get-field directory other-project))
  (parameterize ([polyglot-project-directory tmpdir])
    (define asset-paths (directory-list (assets-rel) #:build? #t))
    (define other-asset-paths
      (parameterize ([polyglot-project-directory other-tmpdir])
        (directory-list (assets-rel) #:build? #t)))

    (define non-asset-paths (directory-list (system-temp-rel) #:build? #t))
    (define mix (list (car asset-paths) (car non-asset-paths)))
    (test-equal? "Preserve all asset paths"
                 (keep-own-asset-paths project asset-paths)
                 asset-paths)
    (test-equal? "Discard all non-asset paths"
                 (keep-own-asset-paths project non-asset-paths)
                 null)
    (test-equal? "Keep only the assets relevant to the stated project"
                 (keep-own-asset-paths project (append asset-paths
                                                       non-asset-paths
                                                       other-asset-paths))
                 asset-paths))
  (delete-directory/files tmpdir)
  (delete-directory/files other-tmpdir))

(test-case "make-polyglot-workflow-object"
  (define workflow (class polyglot/base% (super-new)))
  (define skel-path (car skel-paths))
  (test-true "forced workflows appear are selected unconditionally"
             (is-a? (make-polyglot-workflow-object (new polyglot-project% [directory skel-path])
                                                   #:forced-workflow workflow)
                    workflow))
  (test-case "fallback workflows apply when .polyglotrc.rkt fails to deliver"
    (define tmp-project (copy-polyglot-skeleton-project! #:force? #t "functional"))
    (define tmpdir (get-field directory tmp-project))
    (delete-file (build-path tmpdir ".polyglotrc.rkt"))
    (check-true (is-a? (make-polyglot-workflow-object tmp-project
                                                      #:fallback-workflow workflow)
                       workflow))
    (delete-directory/files tmpdir))
  (test-case "in normal cases, workflows come from .polyglotrc.rkt."
    (define project (new polyglot-project% [directory skel-path]))
    (check-true (is-a? (make-polyglot-workflow-object project)
                       (send project get-workflow-class)))))

(for ([path skel-paths])
  (define-values (base name _) (split-path path))
  (test-case (format "Can build ~a" name)
    ((make-polyglot-builder path #:entry-assets '("index.md")))
    (parameterize ([polyglot-project-directory path])
      (check-equal? (length (directory-list (assets-rel)))
                    (length (directory-list (dist-rel))))
      (delete-directory/files (dist-rel)))))
