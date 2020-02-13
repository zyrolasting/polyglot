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


(define (find-closest-project path)
  (if (useable-polyglot-directory? path)
      (new polyglot-project% [directory (simplify-path path)])
      (let ([up (simplify-path (build-path path 'up))])
        (if (equal? up path)
            #f
            (find-closest-project up)))))

(define (copy-polyglot-skeleton-project! name
                                         #:force? [force? #f]
                                         [dest (make-temporary-file "polyglot~a"
                                                                    'directory)])
  (when force?
    (delete-directory/files dest))
  (copy-directory/files (polyglot-rel "private" "skel" name)
                        dest)
  (new polyglot-project% [directory dest]))



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
        (string-prefix? (path->string (path->complete-path (simplify-path path)))
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
