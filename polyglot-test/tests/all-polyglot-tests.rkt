#lang racket/base

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         markdown
         unlike-assets
         polyglot)

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


(module+ test
  (test-case
    "Mime-type selections"
    (define (test-pair pair)
      (check-equal? ((path->mime-proc) (build-path (car pair))) (cdr pair)))
    (for ([pair '(("a.html" . "text/html")
                  ("a.css"  . "text/css")
                  ("a.wtf"  . "application/x-unknown-content-type"))])
         (test-pair pair))))


(module+ test
  (test-case "Ephemeral dynamic modules from string content"
    (define module-cdata
        '("#lang racket/base"
          "(provide (all-defined-out))"
          "(define b (read))"
          "(define a 1)"
          "(define c 3)"
          "(write \"o\" (current-output-port))"
          "(write \"e\" (current-error-port))"))

    (define path (make-temporary-file))
    (with-lines->file
      path
      module-cdata
      (lambda ()
        (with-ephemeral-module
          path
          #:input (lambda (port) (write "2" port))
          `(b c a (d . ,(λ _ "thunked")))
          (lambda (b c a d output-port input-port error-port)
            (check-equal? b "2")
            (check-equal? c 3)
            (check-equal? a 1)
            (check-equal? d "thunked")
            (check-equal? (read error-port) "e")
            (check-equal? (read output-port) "o")))))))

(module+ test
  (require rackunit racket/function)
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
      "main.js")))


(module+ test
  (require racket/file
           rackunit)

  (test-case "make-dist-path-string"
    (test-equal? "Produce relative paths as strings given two complete paths"
                 (make-dist-path-string (build-path "/blah" "some/where.rkt")
                                        (build-path "/blah"))
                 "some/where.rkt")
    (test-equal? "Return webroot path if paths are the same"
                 (make-dist-path-string (build-path "/blah/")
                                        (simplify-path (build-path "/blah/" 'same)))
                 "/"))

  (test-case "Users may override derived paths"
    (define (check-fallbacks)
      (check-equal? (polyglot-assets-directory) (build-path (polyglot-project-directory) "assets"))
      (check-equal? (polyglot-dist-directory) (build-path (polyglot-project-directory) "dist")))
    (dynamic-wind
      check-fallbacks
      (λ _
        (parameterize ([polyglot-project-directory (current-directory)]
                       [polyglot-assets-directory (current-directory)]
                       [polyglot-dist-directory (current-directory)])
          (check-equal? (polyglot-assets-directory) (polyglot-dist-directory))
          (check-equal? (polyglot-assets-directory) (polyglot-project-directory))))
      check-fallbacks)))


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

    (define tmpd (make-temp-ephmod-directory))
    (define (group! type)
      (group-scripts! tx
                      tmpd
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
    (delete-directory/files tmpd)))


(define test-script-loading
  (test-suite
   "scripts.rkt"
   (test-case "load-script"
     (define element
       '(script ((id "blah"))
                "#lang racket"
                "(write \"x\" (current-output-port))"
                "(write \"y\" (current-output-port))"
                "(write \"a\" (current-error-port))"
                "(write \"b\" (current-error-port))"))
     (define spath (script->path element (system-temp-rel)))
     (write-script element (system-temp-rel))
     (define-values (o i e) (load-script spath))
     (define-values (fragment errors) (values (port->list read o)
                                              (port->list read e)))
     (check-equal? fragment '("x" "y"))
     (check-equal? errors '("a" "b"))

     (check-true (file-exists? spath))
     (delete-file spath))

   (test-equal? "Derive filesystem paths from script txexpr"
                (script->path '(script ((id "coolio"))) (project-rel))
                (project-rel "coolio.rkt"))))


(define test-polyglot-projects
  (test-suite
   "Polyglot Projects"
   (test-true "There are at least two skeleton projects"
              (>= (length skel-paths) 2)))

  (test-case "useable-polyglot-directory?"
    (for ([path skel-paths])
      (check-true (useable-polyglot-directory? path)))
    (check-false (useable-polyglot-directory? skels)))

  (test-case "find-closest-project"
    (define get-directory (class-field-accessor polyglot-project% directory))
    (for ([fn skel-paths])
      (parameterize ([polyglot-project-directory fn])
        (define obj (find-closest-project (assets-rel "index.md")))
        (check-true (is-a? obj polyglot-project%))
        (check-equal? fn (get-directory obj)))))


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
      (delete-directory/files dir)))


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


(define test-imperative-workflow
  (test-suite
   "Imperative Workflow"
   (define elements
     '((p (script ((id "boo") (type "text/racket"))
                  "#lang racket/base"
                  "(provide foo)"
                  "(define foo 1)"))
       (p "The result is")
       (script ((type "application/rackdown"))
               "#lang racket"
               "(require \"./boo.rkt\")"
               "(provide layout)"
               "(write `(span ,(number->string (+ foo 1))))"
               "(define layout (λ (kids) `(body . ,kids)))")))

   (test-equal? "Rackdown"
                (with-handlers ([exn? displayln]) (run-rackdown elements))
                '(body (p "The result is") (span "2")))

   (test-case
       "Can preprocess tagged X-expressions from asset folder"
     (define provided-name "replacer")
     (define (empty-directory path)
       (delete-directory/files path #:must-exist? #f)
       (make-directory path))

     (define (e2e provided-name)
       (define compiler (new polyglot/imperative%))
       (define module-name (format "macro-~a" provided-name))
       (define tmpdir (system-temp-rel "pg-test"))

       (empty-directory tmpdir)
       (parameterize ([polyglot-project-directory tmpdir])
         (empty-directory (assets-rel))
         (define module-path (assets-rel (format "~a.rkt" module-name)))

         (display-lines-to-file
          `("#lang racket/base"
            "(require txexpr)"
            ,(format "(provide ~a)" provided-name)
            ,(format "(define (~a other)" provided-name)
            "  `((p ,(attr-ref other 'data-foo))))")
          module-path)

         ; We don't want to specify the compiler's fallback name in the input code.
         ; How else would we know if the compiler used the fallback correc
         (define input-code
           `((meta ((itemprop "build")
                    (data-foo "made it")
                    (data-macro ,(format "~a ~a"
                                         module-name
                                         (if (equal? provided-name fallback-provided-name)
                                             ""
                                             provided-name)))))))

         (define changed
           (car (send compiler
                      preprocess-txexprs
                      input-code)))

         (check-true (and (txexpr? changed)
                          (equal? (get-tag changed) 'p)
                          (equal? (car (get-elements changed))
                                  "made it")))))

     (e2e "different-name")
     (e2e fallback-provided-name))

   (test-case "run-txexpr/imperative!"
     (define tmp (make-temporary-file))
     (delete-file tmp)
     (make-directory tmp)
     (parameterize ([current-directory tmp]
                    [polyglot-project-directory tmp])
       (display-lines-to-file
        '("#lang racket/base"
          "(provide datum)"
          "(define datum '(p \"test!\"))")
        "datum.rkt")

       (define rackdown
         (string-join
          '("Hello"
            "<script type=\"text/racket\" id=\"lib\">"
            "#lang racket/base"
            "(require \"project/datum.rkt\")"
            "(provide from-lib)"
            "(define from-lib datum)"
            "</script>"
            "<script type=\"application/rackdown\">"
            "#lang racket/base"
            "(require \"lib.rkt\")"
            "(write from-lib)"
            "</script>")
          "\n"))

       (test-equal?
        "Process Rackdown externally"
        (run-txexpr/imperative! (parse-markdown rackdown)
                                (λ (kids)
                                  `(body . ,kids)))
        '(body (p "Hello") (p "test!")))))))


(define test-polyglot-builds
  (test-suite
   "polyglot-builds"
   (test-case "keep-own-asset-paths"
     (define project (copy-polyglot-skeleton-project! #:force? #t "functional"))
     (define other-project (copy-polyglot-skeleton-project! #:force? #t "functional"))
     (define tmpdir (get-field directory project))
     (define other-tmpdir (get-field directory other-project))
     (parameterize ([polyglot-project-directory tmpdir])
       (define asset-paths (directory-list (assets-rel) #:build? #t))
       (define other-asset-paths
         (parameterize ([polyglot-project-directory other-tmpdir])
           (directory-list (assets-rel) #:build? #t)))
       (define non-asset-paths (directory-list (system-temp-rel) #:build? #t))
       (define mix (list (car asset-paths) (car non-asset-paths)))
       (test-equal? "Preserve all asset paths"
                    (keep-own-asset-paths project asset-paths)
                    asset-paths)
       (test-equal? "Discard all non-asset paths"
                    (keep-own-asset-paths project non-asset-paths)
                    null)
       (test-equal? "Keep only the assets relevant to the stated project"
                    (keep-own-asset-paths project (append asset-paths
                                                          non-asset-paths
                                                          other-asset-paths))
                    asset-paths))
     (delete-directory/files tmpdir)
     (delete-directory/files other-tmpdir))
   (test-case "make-polyglot-workflow-object"
     (define workflow (class polyglot/base% (super-new)))
     (define skel-path (car skel-paths))
     (test-true "forced workflows appear are selected unconditionally"
                (is-a? (make-polyglot-workflow-object (new polyglot-project% [directory skel-path])
                                                      #:forced-workflow workflow)
                       workflow))
     (test-case "fallback workflows apply when .polyglotrc.rkt fails to deliver"
       (define tmp-project (copy-polyglot-skeleton-project! #:force? #t "functional"))
       (define tmpdir (get-field directory tmp-project))
       (delete-file (build-path tmpdir ".polyglotrc.rkt"))
       (check-true (is-a? (make-polyglot-workflow-object tmp-project
                                                         #:fallback-workflow workflow)
                          workflow))
       (delete-directory/files tmpdir))
     (test-case "in normal cases, workflows come from .polyglotrc.rkt."
       (define project (new polyglot-project% [directory skel-path]))
       (check-true (is-a? (make-polyglot-workflow-object project)
                          (send project get-workflow-class)))))

   (test-suite "Build skeleton projects"
               (for ([path skel-paths])
                 (define-values (base name _) (split-path path))
                 (test-case (format "Can build ~a" name)
                   ((make-polyglot-builder path #:entry-assets '("index.md")))
                   (parameterize ([polyglot-project-directory path])
                     (check-equal? (length (directory-list (assets-rel)))
                                   (length (directory-list (dist-rel))))
                     (delete-directory/files (dist-rel))))))))

(define/provide-test-suite all-polyglot-tests
  test-polyglot-builds)
