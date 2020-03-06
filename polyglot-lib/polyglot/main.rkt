#lang racket/base

(define-syntax-rule (r+p spec ...)
  (begin
    (begin
      (require spec)
      (provide (all-from-out spec)))
    ...))

(r+p "./base.rkt"
     "./builds.rkt"
     "./txexpr.rkt"
     "./paths.rkt"
     "./imperative.rkt"
     "./functional.rkt"
     "./projects.rkt"
     "./elements.rkt")

(module reader racket/base
  (provide (rename-out [markup-read read]
                       [markup-read-syntax read-syntax]))

  (require (only-in scribble/reader
                    read-syntax-inside)
           racket/class
           syntax/modread
           "./paths.rkt"
           "./projects.rkt")

  (define (markup-read in)
    (syntax->datum (markup-read-syntax in)))

  (define (markup-read-syntax src in)
    (define stx (read-syntax-inside (object-name in) in))
    (with-syntax ([data (run-markup src stx)])
      #'(module content racket/base
          (provide doc)
          (define doc 'data)
          (module+ main
            (writeln doc)))))

  (define (run-markup src stx)
    (displayln "Entering run-markup")

    (define (mod id . strs)
      (define from-str (open-input-string (apply string-append strs)))
      (define (read-thunk) (read-syntax (object-name from-str) from-str))
      (define user-stx (with-module-reading-parameterization read-thunk))
      (define checked (check-module-form user-stx '_ "inline module"))
      (define datum (syntax->datum checked))
      (define renamed (apply list (car datum) id (cddr datum)))
      (define prepared (datum->syntax checked renamed))
      (define enriched (namespace-syntax-introduce prepared ns))
      (eval-syntax (expand enriched) ns))

    (define (app id provided . strs)
      (apply mod id strs)
      (eval `(begin (require ',id) ,provided) ns))

    (define ns (make-base-namespace))
    (namespace-set-variable-value! 'mod mod #t ns #t)
    (namespace-set-variable-value! 'app app #t ns #t)
    (define enriched (namespace-syntax-introduce stx ns))

    ; Load project bindings into document.
    (define prj (and (path? src) (find-closest-project src)))
    (define dir (if prj (get-field directory prj) (polyglot-project-directory)))
    (define rcfile (build-path dir ".polyglotrc.rkt"))

    (parameterize ([polyglot-project-directory dir])
      (when (file-exists? rcfile)
        (eval `(require ,(path->string rcfile)) ns))

      (reverse
       (for/fold ([acc '()])
                 ([expr (in-list (syntax-e enriched))])
         (define result (eval-syntax expr ns))
         (if (void? result)
             acc
             (cons result acc)))))))
