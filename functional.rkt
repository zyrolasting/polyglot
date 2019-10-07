#lang racket/base

(require racket/contract)
(provide polyglot/functional%
         (contract-out
          [current-replace-element-predicate (parameter/c (-> txexpr? any/c))]
          [run-txexpr/functional! (-> (or/c (non-empty-listof txexpr?))
                                      (non-empty-listof txexpr?))]

          [tx-replace-me (-> txexpr?
                             (-> txexpr?
                                 (listof txexpr-element?))
                             txexpr?)]))

(require racket/class
         racket/file
         racket/function
         racket/list
         racket/path
         racket/rerequire
         racket/string
         unlike-assets
         unlike-assets/logging
         [except-in markdown xexpr->string]
         "./private/base-workflow.rkt"
         "./private/dynamic-modules.rkt"
         "./private/fs.rkt"
         "./private/scripts.rkt"
         "./paths.rkt"
         "./txexpr.rkt")

;; Represent a script group in a page and on the filesystem in one value.
; ------------------------------------------------------------------------
(struct script-info (element path predicate))

(define (group-scripts! tx tmp-rel match?)
  (define matches (findf*-txexpr tx match?))
  (if (list? matches)
      (for/list ([x (in-list matches)])
        (script-info x
                     (write-script x tmp-rel)
                     (λ (other) (and (txexpr? other)
                                     (equal? (attr-ref other 'id #f)
                                             (attr-ref x 'id))))))
      '()))


; Allow user to replace the same script elements in which their code runs.
; ------------------------------------------------------------------------
(define current-replace-element-predicate (make-parameter (λ _ #f)))
(define (tx-replace-me page-tx replacer)
  (tx-replace page-tx
              (current-replace-element-predicate)
              replacer))


; Guarentee unique IDs on relevant script elements.
; ------------------------------------------------------------------------
(define (replace-page/scripts-with-ids page)
  (define all-ids
    (map (λ (with-id) (attr-ref with-id 'id))
         (or (findf*-txexpr page
                            (λ (x) (and (txexpr? x)
                                        (attrs-have-key? x 'id)
                                        (attr-ref x 'id #f))))
             '())))

  (define longest-id
    (for/fold ([longest ""])
              ([id (in-list all-ids)])
      (if (> (string-length id)
             (string-length longest))
          id
          longest)))

  (<debug "Longest ID to be used as prefix: ~a" longest-id)

  (tx-replace
   page
   (λ (x) (and (or (app-script? x)
                   (lib-script? x))
               (= (string-length (attr-ref x 'id "")) 0)))
   (λ (x)
     (list (attr-set x
                     'id
                     (string-append longest-id
                                    (symbol->string (gensym))))))))

; Allow user to replace page using an app element.
; ------------------------------------------------------------------------
(define (replace-page/user-defined page path predicate)
  (parameterize ([current-replace-element-predicate predicate])
    (<info "Applying replace-page in ~a" path)
    ((dynamic-require path
                      'replace-page
                      (λ _ identity))
     page)))

; These are for hygienic post-processing, and to help prevent infinite
; loops due to straggling script elements that have already been used.
; ------------------------------------------------------------------------
(define (replace-page/remove-all page predicates)
  (for/fold ([pruned page])
            ([p predicates])
    (tx-replace pruned p (λ _ null))))

(define (replace-page/no-empty-paragraphs page)
  (tx-replace
   page
   (λ (x) (and (tag-equal? 'p x)
               (= (length (get-elements x)) 0)))
   (λ _ null)))


; Applies functional workflow to content
; ------------------------------------------------------------------------
(define (pass page tmpd)
  (define initial-page (replace-page/scripts-with-ids page))
  (define apps (group-scripts! initial-page tmpd app-script?))
  (define libs (group-scripts! initial-page tmpd lib-script?))

  ; Shape workflow such that (listen) gets meaningful feedback.
  (define steps
    (append
     (map (λ (path predicate)
            (λ (page)
              (replace-page/user-defined page
                                         path
                                         predicate)))
          (map script-info-path apps)
          (map script-info-predicate apps))
     (list (λ (page)
             (replace-page/remove-all
              page
              (map script-info-predicate (append apps libs))))
           replace-page/no-empty-paragraphs)))

  (for/fold ([before page])
            ([step (in-list steps)])
    (step before)))

(define (run-txexpr/functional! target
                                #:max-passes [max-passes 1000])
  (define page (if (txexpr? target)
                   target
                   (make-minimal-html-page target)))

  (define tmpd (make-temp-ephmod-directory))

  (dynamic-wind
    void
    (λ _ (let loop ([page/before page] [on-pass max-passes])
           (when (<= on-pass 0)
             (error "Maximum passes exceeded."))

           (<debug "Pass ~a" on-pass)

           (define page/after (pass page/before tmpd))
           (if (not (equal? page/before page/after))
               (loop page/after (sub1 on-pass))
               page/after)))
    (λ _ (delete-directory/files (tmpd)))))


(define polyglot/functional%
  (class* polyglot/base% () (super-new)
    (define/public (preprocess-page page-tx) page-tx)
    (define/public (postprocess-page page-tx) page-tx)
    (define/override (delegate path)
      (case (path-get-extension path)
        [(#".md")
         (λ (clear compiler)
           (define fragment (parse-markdown clear))
           (define base-page (make-minimal-html-page fragment))
           (define preprocessed (preprocess-page base-page))
           (add-dependencies!
            clear
            compiler
            (postprocess-page (run-txexpr/functional! preprocessed))))]
        [else (super delegate path)]))))

(module+ test
  (require racket/function
           rackunit)

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

    (define tmp-rel (make-temp-ephmod-directory))
    (define (group! type)
      (group-scripts! tx
                      tmp-rel
                      (λ (x) (and (txexpr? x)
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
    (delete-directory/files (tmp-rel))))
