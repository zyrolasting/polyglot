#lang racket/base

;;; Provides operations used in this project for tagged X-expressions

(require racket/contract
         txexpr
         xml)

(define txe-predicate/c (-> txexpr-element? any/c))
(define replacer/c (-> txexpr-element? (listof txexpr-element?)))
(define transformed/c (or/c (listof txexpr-element?) txexpr?))
(define replaced/c (listof txexpr?))

(provide (all-from-out txexpr xml)
         (contract-out [tag-equal?
                        (-> symbol?
                            any/c
                            boolean?)]
                       [make-tag-predicate
                        (-> (non-empty-listof symbol?)
                            (-> any/c boolean?))]
                       [substitute-many-in-txexpr
                        (-> txexpr?
                            txe-predicate/c
                            replacer/c
                            (values transformed/c replaced/c))]
                       [substitute-many-in-txexpr/loop
                        (->* (txexpr?
                              txe-predicate/c
                              replacer/c)
                             (#:max-replacements exact-integer?)
                             (values transformed/c replaced/c))]
                       [interlace-txexprs
                        (->* ((non-empty-listof txexpr?)
                              (or/c txe-predicate/c
                                    (non-empty-listof txe-predicate/c))
                              (or/c replacer/c
                                    (non-empty-listof replacer/c)))
                             (#:max-replacements exact-integer?
                              #:max-passes exact-integer?)
                             (non-empty-listof txexpr?))]))

(define (tag-equal? t tx)
  (and (txexpr? tx)
       (equal? t (get-tag tx))))

(define (make-tag-predicate tags)
  (λ (tx) (ormap (λ (t) (tag-equal? t tx))
                 tags)))

;; Like splitf-txexpr, except each matching element can be replaced by
;; more than one element. Elements left without children as a result
;; of a replacement can be preserved or discarded.
(define (substitute-many-in-txexpr tx replace? replace)
  (define (has-target-children? x)
    (and (list? x)
         (findf replace? (get-elements x))
         #t))

  (define (replace-children x)
    (txexpr
     (get-tag x)
     (get-attrs x)
     (foldl
      (λ (kid res)
        (append res
                (if (replace? kid)
                    (replace kid)
                    (list kid))))
      null
      (get-elements x))))

  (if (replace? tx)
      (values (replace tx) (list tx))
      (splitf-txexpr tx has-target-children? replace-children)))


;; Like substitute-many-in-txexpr/once, except control will not
;; leave the procedure until all possible replacements are made.
(define (substitute-many-in-txexpr/loop tx replace? replace
                                   #:max-replacements [max-replacements 1000])
  (when (<= max-replacements 0)
    (error 'substitute-many-in-txexpr
           "Maximum replacements exceeded."))

  (define-values (new-content replaced)
    (substitute-many-in-txexpr tx replace? replace))

  (if (> (length replaced) 0)
      (let-values ([(new-content/next replaced/next)
                    (substitute-many-in-txexpr/loop
                     new-content
                     replace?
                     replace
                     #:max-replacements (sub1 max-replacements))])
        (values new-content/next
                (append replaced replaced/next)))
      (values new-content
              replaced)))


;; A "loom" for tagged x-expressions that coordinates calls to
;; substitute-many-in-txexpr.
(define (interlace-txexprs elements
                           replace?
                           replace
                           #:max-replacements [max-replacements 1000]
                           #:max-passes [max-passes 50])
  (when (<= max-passes 0)
    (error 'interlace-txexprs "Maximum passes exceeded"))

  (define (normalize-argument v)
    (if (list? v) v (list v)))

  (define replace?/normalized (normalize-argument replace?))
  (define replace/normalized (normalize-argument replace))
  (define replace?/len (length replace?/normalized))
  (define replace/len (length replace/normalized))

  (when (not (= replace?/len replace/len))
    (raise-arguments-error 'interlace-txexprs
                           "Mismatch in number of replace? and replace procedures."
                           "# of replace? procedures" replace?/len
                           "# of replace procedures" replace/len))

  ;; A mock root element created with (gensym) lets us treat top-level
  ;; elements like subtrees, simplifying the problem.
  (define-values (under-mock-root need-new-pass?)
    (for/fold ([new-elements (cons (gensym) elements)]
               [any-replacements? #f])
              ([replace? replace?/normalized]
               [replace replace/normalized])
      (define-values (new-content replaced)
        (substitute-many-in-txexpr/loop
         #:max-replacements max-replacements
         new-elements
         replace?
         replace))
      (values new-content
              (or any-replacements?
                  (> (length replaced) 0)))))

  (define transformed (get-elements under-mock-root))

  (if need-new-pass?
      (interlace-txexprs transformed
                         replace?/normalized
                         replace/normalized
                         #:max-replacements max-replacements
                         #:max-passes (sub1 max-passes))
      transformed))


(module+ test
  (require rackunit racket/function)
  (define i-element? (curry tag-equal? 'i))
  (define s-element? (curry tag-equal? 's))

  (test-exn "Length mismatch throws a contract error"
            exn:fail:contract?
            (λ _ (interlace-txexprs
                  '((whatever))
                  (list (λ _ _) (λ _ _))
                  (λ _ _))))

  (test-equal? "Match/replace fragments using two procedures"
    (interlace-txexprs
      '((i) (p (i) (i)) (b) (s) (b (i)) (i))
      (list i-element?
            s-element?)
      (list (λ _ '((b) (b)))
            (λ _ '((x) (x) (x)))))
    '((b) (b) (p (b) (b) (b) (b)) (b) (x) (x) (x) (b (b) (b)) (b) (b))))
