#lang racket/base

(require racket/contract)
(provide (contract-out
          [polyglot-project% (class/c [asset-path? (->m path? boolean?)]
                                      [get-workflow-class (->m (-> any) class?)]
                                      (field [directory useable-polyglot-directory?]))]
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

    (define/public (asset-path? path)
      (parameterize ([polyglot-project-directory directory])
        (string-prefix? (path->string (path->complete-path path))
                        (path->string (assets-rel)))))

    (define/public (get-workflow-class [fail-thunk (λ _ (error 'polyglot
                                                               "Failed to load ~a"
                                                               polyglot-rcfile-name))]
                                       #:live? [live? #f])
      (define rcfile (build-path directory polyglot-rcfile-name))
      (when live? (dynamic-rerequire rcfile))
      (if (file-exists? rcfile)
          (dynamic-require rcfile 'polyglot+% fail-thunk)
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

(module+ test
  (for ([path skel-paths])
    (define-values (base name _) (split-path path))
    (test-case (format "Can build ~a" name)
      (parameterize ([polyglot-project-directory path])
        (define project (new polyglot-project% [directory path]))
        (define compiler (new (send project get-workflow-class)))
        (send project ensure-empty-distribution!)
        (send compiler add! (assets-rel "index.md"))
        (send compiler compile!)
        (check-equal? (length (directory-list (assets-rel)))
                      (length (directory-list (dist-rel))))
        (delete-directory/files (dist-rel))))))