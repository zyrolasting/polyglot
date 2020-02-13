#lang racket/base

(require rackunit
         racket/function
         (submod polyglot/txexpr safe))

(define i-element? (curry tag-equal? 'i))
(define s-element? (curry tag-equal? 's))

(test-equal? "Search for tagged element"
             (tx-search-tagged
              '(body (a ((i "1")))
                     (b (a ((i "2")))
                        (a ((i "3"))))
                     (a ((i "4"))
                        (a ((i "5")))))
              'a)
             '((a ((i "1")))
               (a ((i "2")))
               (a ((i "3")))
               (a ((i "4"))
                  (a ((i "5"))))))

(test-equal? "Search produces empty list on no matches"
             (tx-search-tagged '(body (b)) 'a)
             '())

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

(test-equal? "Manifest can replace `src`, `href`, and `srcset` attributes"
             (apply-manifest '(html
                               (head
                                (link ((href "a.css"))))
                               (body
                                (picture
                                 (source ((srcset "b.png")
                                          (media "(min-width: 1280px)")))
                                 (img ((src "c.png"))))))
                             '(("a.css" . "123.css")
                               ("b.png" . "456.png")
                               ("c.png" . "789.png")))
             '(html
               (head
                (link ((href "123.css"))))
               (body
                (picture
                 (source ((media "(min-width: 1280px)")
                          (srcset "456.png")))
                 (img ((src "789.png")))))))

(test-equal? "apply-manifest allows custom rewrites"
             (apply-manifest '(html
                               (head
                                (link ((href "a.css"))))
                               (body
                                (img ((src "b.png")))))
                             '(("a.css" . "123.css")
                               ("b.png" . "456.png"))
                             string-upcase)
             '(html
               (head
                (link ((href "123.CSS"))))
               (body
                (img ((src "456.PNG"))))))

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
                            (picture (source ((srcset "hurp.png")))
                                     (img ((src "mark.png"))))))
                        (section
                         (h1 "Heading")
                         (a ((href "/path/to/image.svg")) "Click to view")))
                       (a ((href "#top")) "Back to top"))
                      (script ((type "text/javascript") (src "main.js"))))))
             '("blah.css"
               "hurp.png"
               "mark.png"
               "/path/to/image.svg"
               "main.js"))
