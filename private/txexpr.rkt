#lang racket/base

;;; Provides operations used in this project for tagged X-expressions

(require
  (submod txexpr safe)
  xml)

(provide
  (all-from-out
    (submod txexpr safe)
    xml)
  (all-defined-out))

(define (tag-equal? t tx)
  (and (txexpr? tx)
       (equal? t (get-tag tx))))

;; Like splitf-txexpr, except:
;;   - Each matching element can be replaced by
;;     more than one element.
;;   - Control will not leave the procedure
;;     until all possible replacements are made.
(define (refoster tx replace? replace)
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

  (let-values ([(new-content replaced)
                (splitf-txexpr
                 tx
                 has-target-children?
                 replace-children)])
    (if (> (length replaced) 0)
        (refoster new-content replace? replace)
        new-content)))

;; *: A mock root element lets us treat top-level
;; elements like subtrees, simplifying the problem.
(define (interlace-txexpr elements replace?/list replace/list)
  (define elements/after
    (get-elements
     (foldl
      (λ (replace? replace tx)
        (refoster tx
                  replace?
                  replace))
      (cons 'root elements) ; *
      replace?/list
      replace/list)))

  ; Optimize by tracking if a replacement
  ; occurred if performance becomes an issue.
  (if (not (equal? elements elements/after))
      (interlace-txexpr elements/after replace?/list replace/list)
      elements/after))


(module+ test
  (require rackunit racket/function)
  (define i-element? (curry tag-equal? 'i))
  (define s-element? (curry tag-equal? 's))

  (test-equal? "interlace-txexpr: one pass"
    (interlace-txexpr
      '((i) (p (i) (i)) (b) (s) (b (i)) (i))
      (list i-element?
            s-element?)
      (list (λ _ '((b) (b)))
            (λ _ '((x) (x) (x)))))
    '((b) (b) (p (b) (b) (b) (b)) (b) (x) (x) (x) (b (b) (b)) (b) (b))))
