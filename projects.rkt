#lang racket/base

(require racket/contract)
(provide (contract-out
          [polyglot-project% (class/c [asset-path? (->m path? boolean?)]
                                      [get-workflow-class (->*m () ((-> any) #:live? boolean?) class?)]
                                      (field [directory useable-polyglot-directory?]))]
          [copy-polyglot-skeleton-project!
           (->* ((or/c "functional" "imperative"))
                ((or/c path-string?
                       path?)
                 #:force? any/c)
               (is-a?/c polyglot-project%))]
          [useable-polyglot-directory? (-> path? boolean?)]
          [find-closest-project (-> path? (or/c #f (is-a?/c polyglot-project%)))]))

; Named to protect against typos.
(define polyglot-rcfile-name ".polyglotrc.rkt")


; -----------------

(require racket/class
         racket/file
         racket/string
         racket/rerequire
         "paths.rkt")

(module+ test
  (provide skel-paths)
  (require racket/runtime-path
           racket/list
           rackunit
           unlike-assets
           "private/fs.rkt")
  ; Testing against the project templates is dogfooding.
  (define-runtime-path skels "./private/skel")

  (define skel-paths
    (filter-map
     (λ (name)
       (define path (build-path skels name))
       (and (directory-exists? path)
            ; The extra "." provides a trailing directory separator
            ; we'll need for assertions.
            (simplify-path (build-path path "."))))
     (directory-list skels)))

  (test-true "There are at least two skeleton projects"
             (>= (length skel-paths) 2)))


(define (useable-polyglot-directory? directory)
  (with-handlers ([exn:fail? (λ _ #f)])
    (parameterize ([polyglot-project-directory directory])
      (let ([dperms (file-or-directory-permissions directory)]
            [aperms (file-or-directory-permissions (assets-rel))])
        (and (member 'read dperms)
             (member 'write dperms)
             (member 'read aperms)
             ; The rcfile doesn't have to exist, but if it does, it must be readable.
             (let ([rcpath (build-path directory polyglot-rcfile-name)])
               (or (not (file-exists? rcpath))
                   (member 'read (file-or-directory-permissions rcpath))))
             #t)))))

(module+ test
  (test-case "useable-polyglot-directory?"
    (for ([path skel-paths])
      (check-true (useable-polyglot-directory? path)))
    (check-false (useable-polyglot-directory? skels))))

(define (find-closest-project path)
  (if (useable-polyglot-directory? path)
      (new polyglot-project% [directory (simplify-path path)])
      (let ([up (simplify-path (build-path path 'up))])
        (if (equal? up path)
            #f
            (find-closest-project up)))))

(module+ test
  (test-case "find-closest-project"
    (define get-directory (class-field-accessor polyglot-project% directory))
    (for ([fn skel-paths])
      (parameterize ([polyglot-project-directory fn])
        (define obj (find-closest-project (assets-rel "index.md")))
        (check-true (is-a? obj polyglot-project%))
        (check-equal? fn (get-directory obj))))))


(define (copy-polyglot-skeleton-project! name
                                         #:force? [force? #f]
                                         [dest (make-temporary-file "polyglot~a"
                                                                    'directory)])
  (when force?
    (delete-directory/files dest))
  (copy-directory/files (polyglot-rel "private" "skel" name)
                        dest)
  (new polyglot-project% [directory dest]))

(module+ test
  (test-case "copy-polyglot-skeleton-project!"
    (test-case "can create useable projects as a result of the copy"
      (for ([path skel-paths])
        (define-values (base name _) (split-path path))
        (define project
          (copy-polyglot-skeleton-project! #:force? #t
                                           name
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
      (delete-directory/files dir))))



(define polyglot-project%
  (class* object% (equal<%>)
    (super-new)
    (init-field directory)

    (define/public (equal-to? other recur)
      (equal? (simplify-path directory)
              (simplify-path (get-field directory other))))
    (define/public (equal-hash-code-of hash-code)
      (hash-code (path->string (simplify-path directory))))
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code (path->string (simplify-path directory))))

    (define/public (get-directory-name)
      (define-values (base name _) (split-path directory))
      name)

    (define/public (asset-path? path)
      (parameterize ([polyglot-project-directory directory])
        (string-prefix? (path->string (path->complete-path path))
                        (path->string (assets-rel)))))

    (define/public (get-workflow-class [fail-thunk (λ _ (error 'polyglot
                                                               "Failed to load ~a"
                                                               polyglot-rcfile-name))]
                                       #:live? [live? #f])
      (define rcfile (build-path directory polyglot-rcfile-name))
      (if (file-exists? rcfile)
          (begin
            (when live? (dynamic-rerequire rcfile))
            (dynamic-require rcfile 'polyglot+% fail-thunk))
          (fail-thunk)))

    (define/public (ensure-empty-distribution!)
      (parameterize ([polyglot-project-directory directory])
        (delete-directory/files (dist-rel) #:must-exist? #f)
        (make-directory* (dist-rel))))))


(module+ test
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
        (test-true "Dist directory is empty" (= 0 (length (directory-list (dist-rel)))))))))
