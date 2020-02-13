#lang racket/base

(require racket/class
         racket/contract
         racket/dict
         unlike-assets
         "./projects.rkt"
         "./paths.rkt")

(provide
 (contract-out
  [polyglot-live? (parameter/c boolean?)]
  [make-polyglot-workflow-object (->* ((is-a?/c polyglot-project%))
                                      (#:live? boolean?
                                       #:forced-workflow (or/c #f (subclass?/c unlike-compiler%))
                                       #:fallback-workflow (or/c #f (subclass?/c unlike-compiler%)))
                                      (is-a?/c unlike-compiler%))]
  [build-polyglot-project! (->* ((is-a?/c polyglot-project%)
                                 (is-a?/c unlike-compiler%))
                                (#:changed (listof clear/c)
                                 #:removed (listof clear/c))
                                dict?)]
  [make-polyglot-builder (->* (useable-polyglot-directory?)
                              (#:entry-assets (listof unclear/c)
                               #:forced-workflow (or/c #f (subclass?/c unlike-compiler%))
                               #:fallback-workflow (or/c #f (subclass?/c unlike-compiler%))
                               #:cycle-after (or/c #f exact-positive-integer?)
                               #:gc-request (or/c 'major 'minor 'incremental))
                              (->* ()
                                   (#:changed (listof clear/c)
                                    #:removed (listof clear/c))
                                   dict?))]))

(define polyglot-live? (make-parameter #f))

(define (keep-own-asset-paths project paths)
  (filter (λ (path) (send project asset-path? path))
          paths))



(define (build-polyglot-project! project compiler #:changed [changed '()] #:removed [removed '()])
  (parameterize ([polyglot-project-directory (get-field directory project)])
    (unless (directory-exists? (dist-rel))
      (send project ensure-empty-distribution!))
    (send compiler
          compile!
          #:changed (keep-own-asset-paths project changed)
          #:removed (keep-own-asset-paths project removed))))

(define (make-polyglot-workflow-object project
                                       #:live? [live? #t]
                                       #:forced-workflow [polyglot+% #f]
                                       #:fallback-workflow [fallback-workflow #f])
  (parameterize ([polyglot-project-directory (get-field directory project)])
    (new (or polyglot+%
             (send project
                   get-workflow-class
                   #:live? live?
                   (λ _ (if fallback-workflow
                            fallback-workflow
                            (error "No workflow class found for project: ~a"
                                   (send project get-directory-name)))))))))

(define (make-polyglot-builder project-directory
                               #:forced-workflow [forced-workflow #f]
                               #:fallback-workflow [fallback-workflow #f]
                               #:entry-assets [entry-assets '("index.md")]
                               #:cycle-after [cycle-after 100]
                               #:gc-request [gc-request 'major])
  (define project (new polyglot-project% [directory project-directory]))
  (define (make-fresh-compiler)
    (define instance
      (make-polyglot-workflow-object project
                                     #:live? #t
                                     #:forced-workflow forced-workflow
                                     #:fallback-workflow fallback-workflow))
    (for ([unclear entry-assets])
      (define clear
        (parameterize ([polyglot-project-directory (get-field directory project)])
          (send instance clarify unclear)))
      (unless (send project asset-path? clear)
        (raise-argument-error 'make-polyglot-builder
                              (format "unclear asset name belonging to ~a"
                                      (send project get-directory-name))
                              unclear))
      (send instance add! clear))
    instance)

  (define compiler (make-fresh-compiler))
  (define build-number 0)

  (λ (#:changed [changed null] #:removed [removed null])
    (parameterize ([polyglot-project-directory (get-field directory project)])
      ; Counteract the effects of entropy in the unlike-assets build graph.
      (when (and (> build-number 0) (equal? (modulo build-number cycle-after) 0))
        (set! compiler (make-fresh-compiler))
        (when gc-request (collect-garbage gc-request)))

      (begin0
        (build-polyglot-project! project
                                 compiler
                                 #:changed changed
                                 #:removed removed)
        (set! build-number (add1 build-number))))))
