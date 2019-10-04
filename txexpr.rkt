#lang racket/base

;;; Provides operations used in this project for tagged X-expressions

(require racket/contract
         racket/dict
         racket/function
         racket/list
         racket/sequence
         racket/string
         net/url
         txexpr
         unlike-assets/policy
         xml)

(define txe-predicate/c (-> txexpr-element? any/c))
(define replacer/c (-> txexpr-element? (listof txexpr-element?)))
(define transformed/c (or/c (listof txexpr-element?) txexpr?))
(define replaced/c (listof txexpr?))

(provide (all-from-out txexpr xml)
         (contract-out [genid (-> txexpr?
                                  string?)]
                       [tag-equal?
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
                       [tx-replace
                        (-> txexpr?
                            txe-predicate/c
                            (-> txexpr-element? (listof txexpr?))
                            txexpr?)]
                       [tx-replace-tagged
                        (-> txexpr?
                            symbol?
                            (-> txexpr-element? (listof txexpr?))
                            txexpr?)]
                       [interlace-txexprs
                        (->* ((or/c txexpr?
                                    (non-empty-listof txexpr?))
                              (or/c txe-predicate/c
                                    (non-empty-listof txe-predicate/c))
                              (or/c replacer/c
                                    (non-empty-listof replacer/c)))
                             (#:max-replacements exact-integer?
                              #:max-passes exact-integer?)
                             (non-empty-listof txexpr?))]
                       [discover-dependencies (-> txexpr?
                                                  (listof string?))]
                       [apply-manifest (-> txexpr?
                                           dict?
                                           txexpr?)]))

(define (genid tx)
  (define all-ids
    (map (λ (with-id) (attr-ref with-id 'id))
         (or (findf*-txexpr tx
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

  ; TODO: Use as few characters as possible.
  ; Consider probabalistic approach.
  (string-append longest-id (symbol->string (gensym))))

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

  (define elements/normalized (if (txexpr? elements) (list elements) elements))
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
    (for/fold ([new-elements (cons (gensym) elements/normalized)]
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

(define (tx-replace tx replace? replace)
  (let-values ([(next _)
                (substitute-many-in-txexpr/loop tx
                                                replace?
                                                replace)])
    next))

(define (tx-replace-tagged tx t replace)
  (tx-replace tx
              (λ (x) (tag-equal? t x))
              replace))


(define (get-dependency-ref node)
  (attr-ref node 'src
            (lambda _
              (attr-ref node 'href
                        (lambda _ #f)))))

(define (get-dependency-key x)
  (or (and (attrs-have-key? x 'src) 'src)
      (and (attrs-have-key? x 'href) 'href)
      #f))

(define (discover-dependencies x)
  (map
    get-dependency-ref
    (or
      (findf*-txexpr x
        (lambda (node)
          (and (txexpr? node)
               (let ([ref (get-dependency-ref node)])
                 (and (not (equal? ref "/")) 
                      (local-asset-url? (get-dependency-ref node)))))))
      null)))

(define (apply-manifest content manifest)
  (define (asset-basename path)
    (define-values (base name must-be-dir?)
      (split-path path))
    name)

  (sequence-fold
    (λ (result k v)
      (let-values ([(next _)
                    (splitf-txexpr
                      result
                      (λ (node)
                        (with-handlers ([exn:fail? (thunk* #f)])
                          (equal? (attr-ref node (get-dependency-key node))
                                  k)))
                      (λ (node)
                        (attr-set node
                                  (get-dependency-key node)
                                  (asset-basename v))))])
        next))
    content
    (in-dict manifest)))


(module+ test
  (require rackunit racket/function)
  (define i-element? (curry tag-equal? 'i))
  (define s-element? (curry tag-equal? 's))

  (test-case
    "Output does not change when normalizing arguments"
    (define node '(i))
    (check-equal? (interlace-txexprs node
                                     i-element?
                                     (λ _ '((b))))
                  (interlace-txexprs `(,node)
                                     i-element?
                                     (λ _ '((b))))))

  (test-exn "Length mismatch throws a contract error"
            exn:fail:contract?
            (λ _ (interlace-txexprs
                  '((whatever))
                  (list (λ _ _) (λ _ _))
                  (λ _ _))))

  (test-exn "Prevent exceeding max passes"
            exn:fail?
            (λ _ (interlace-txexprs #:max-passes 2
                                    '((a))
                                    (list (λ _ #t) (λ _ #t))
                                    (list (λ _ '((b))) (λ _ '((a)))))))

  (test-exn "Prevent exceeding max replacements"
            exn:fail?
            (λ _ (substitute-many-in-txexpr/loop
                  #:max-replacements 10
                  '((a))
                  (λ _ #t)
                  (λ _ '((a))))))

  (test-equal? "Match/replace fragments using two procedures"
    (interlace-txexprs
      '((i) (p (i) (i)) (b) (s) (b (i)) (i))
      (list i-element?
            s-element?)
      (list (λ _ '((b) (b)))
            (λ _ '((x) (x) (x)))))
    '((b) (b) (p (b) (b) (b) (b)) (b) (x) (x) (x) (b (b) (b)) (b) (b)))

  (test-equal? "Manifest can replace `src` and `href` attributes"
    (apply-manifest '(html
                      (head
                       (link ((href "a.css"))))
                      (body
                       (img ((src "b.png")))))
                    '(("a.css" . "123.css")
                      ("b.png" . "456.png")))
    '(html
      (head
       (link ((href "123.css"))))
      (body
       (img ((src "456.png"))))))

  (test-equal? "Discover dependencies within a txexpr"
    (discover-dependencies
     '(html
       (head
        (link ((href "blah.css"))))
       (body ((id "top"))
             (main
              (article
               (section
                (h1 "Heading")
                (a ((href "https://example.com"))
                   (img ((src "mark.png")))))
               (section
                (h1 "Heading")
                (a ((href "/path/to/image.svg")) "Click to view")))
              (a ((href "#top")) "Back to top"))
             (script ((type "text/javascript") (src "main.js"))))))
    '("blah.css"
      "mark.png"
      "/path/to/image.svg"
      "main.js")))
