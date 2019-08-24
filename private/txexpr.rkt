#lang racket/base

;;; Provides operations used in this project for tagged X-expressions

(require
  racket/function
  racket/list
  (submod txexpr safe)
  xml)

(provide
  expand-forest
  (all-from-out
    (submod txexpr safe)
    xml)
  splitf-txexpr/root
  has-target-children?
  expand-children)

(define (has-target-children? target? x)
  (and (list? x)
       (findf target? (get-elements x))
       #t))

(define (expand-children target? expand x)
  (txexpr 
    (get-tag x)
    (get-attrs x)
    (foldl
      (λ (kid res)
         (append res
                 (if (target? kid)
                     (expand kid)
                     (list kid))))
      null
      (get-elements x))))

;; splitf-txexpr breaks when replacing the root, so handle that case here.
(define (splitf-txexpr/root tx match? [replace-proc (λ _ #f)])
  (if (match? tx)
      (values (replace-proc tx) (cons tx null))
      (splitf-txexpr tx match? replace-proc)))


#|
Replace one node with several within a containing node.

(expand-nested-target '((p (i) (strong)) (p (i)))
                      (λ (x) (equal? (get-tag x) 'i))
                      (λ (x) '((b) (b))))

yields: '((p (b) (b) (strong)) (p (b) (b)))
|#
(define (expand-nested-target-iter initial-xexpr process? process)
  (let loop ([prev initial-xexpr])
    (if (txexpr? prev)
      (let-values ([(next replaced)
                    (splitf-txexpr/root prev process? process)])
        (if (> (length replaced) 0) (loop next) next))
      #f)))

(define (expand-nested-target elements target? expand)
  (define process? (curry has-target-children? target?))
  (define (process node)
    (define element (expand-children target? expand node))
    (if (empty? (get-elements element))
        #f
        element))
  (foldl
    (λ (initial-xexpr res)
      (define expanded (expand-nested-target-iter initial-xexpr
                                                  process?
                                                  process))
      (if expanded
          (append res (list expanded))
          res))
    null
    elements))


#|
Replace one node with several at the top level.

(expand-nested-target '((i) (b) (i))
                      (λ (x) (equal? (get-tag x) 'i))
                      (λ (x) '((b) (b))))

yields: '((b) (b) (b) (b) (b))
|#
(define (expand-top-level-iter elements target? expand)
  (define replaced? #f)
  (define expanded
    (foldl
      (λ (tx res)
         (append res
           (if (target? tx)
               (begin (set! replaced? #t)
                      (expand tx))
               (list tx))))
      null
      elements))

  (values expanded replaced?))

(define (expand-top-level elements target? expand)
  (let loop ([prev elements])
    (define-values (expanded replaced?) (expand-top-level-iter prev target? expand))
    (if replaced? (loop expanded) expanded)))

;; Combines above cases into one call.
(define (expand-forest elements target? expand)
  (expand-nested-target
    (expand-top-level elements target? expand)
    target?
    expand))

(module+ test
  (require rackunit)
  (define (i-element? x)
    (and (txexpr? x) (equal? (get-tag x) 'i)))

  (test-case "Existential check of target child elements"
    (check-true  (has-target-children? i-element? '(span "a" (i) "b")))
    (check-false (has-target-children? i-element? '(span "a" "b"))))

  (test-equal? "Replace target with nothing"
    (expand-children
      i-element?
      (λ _ null)
      '(span "a" (i) "b" (i) (i) "c" (i)))
    '(span "a" "b" "c"))

  (test-equal? "Replace target with fragments"
    (expand-children
      i-element?
      (λ _ '((b) (b)))
      '(span "a" (i) "b" (i) "c" (i)))
    '(span "a" (b) (b) "b" (b) (b) "c" (b) (b)))


  (test-equal? "Replace target with fragments in element forest"
    (expand-forest
      '((i) (p (i) (i)) (b) (i) (b (i)) (i))
      i-element?
      (λ _ '((b) (b))))
    '((b) (b) (p (b) (b) (b) (b)) (b) (b) (b) (b (b) (b)) (b) (b))))

