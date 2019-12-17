#lang scribble/manual

@require[@for-label[racket polyglot]]

@title{@tt{polyglot/elements}}

@defmodule[polyglot/elements]

@racketmodname[polyglot/elements] integrates the Racket module system
with tagged X-expressions via @racket[dynamic-require].

@defproc[(script-element? [tx any/c]) boolean?]{
Returns @racket[#t] if @racket[tx] is a tagged X-expression with tag
@racket['script].
}

@defproc[(script-element-of-type? [type string?] [tx any/c]) boolean?]{
Returns @racket[#t] if @racket[(script-element? tx)] is @racket[#t] and the
@tt{type} attribute equals @racket[type].
}

@defproc[(app-element? [tx any/c]) boolean?]{
Equivalent to @racketblock[
  (or (script-element-of-type? "application/rackdown" x)
      (script-element-of-type? "application/racket" x))]
}

@defproc[(lib-element? [tx any/c]) boolean?]{
Equivalent to @racket[(script-element-of-type? "text/racket" tx)].
}

@defproc[(app-or-lib-element? [tx any/c]) boolean?]{
Equivalent to @racketblock[(or (lib-element? x)
                               (app-element? x))]
}

@defproc[(write-script [script script-element?] [dir directory-exists?]) path?]{
For use with @racket[load-script].

Writes the text children of @racket[script] to a file in @racket[dir]
and returns a path to the created file.

If @racket[script] contains multiple separate strings as children,
then they will be separated by newline characters in the output file.

Side-effect: Sends @racket["Wrote script: ~a"] info-level event to
@racket[unlike-assets-logger], where @tt{~a} is the displayed
form of the returned path.
}

@defproc[(load-script [path path?] [make-input (-> output-port? any) void]) (values input-port? output-port? input-port?)]{
For use with @racket[write-script].

Like @racket[(dynamic-require path #f)], except any use of
@racket[current-output-port], @racket[current-input-port], or
@racket[current-error-port] are reflected in the returned
ports. @racket[load-script] will apply @racket[make-input] to an output port
before loading the module to populate a buffer.  That buffer may be consumed
via @racket[current-input-port] in the module's top-level forms.

@racketblock[
(define-values (readable-stdout writeable-stdin readable-stderr)
  (load-script (write-script '(script ((id "blah"))
                             "#lang racket"
                             "(displayln \"What's your name?\")"
                             "(define name (read-line))"
                             "(printf \"Hi, ~a!\" name)")
                             (current-directory))
               (λ (to-module)
                 (displayln "Sage" to-module))))

(displayln (read-line readable-stdout))
(displayln (read-line readable-stdout))
]

Take care to note that if the module found at @racket[path] waits for
input, you will need to provide it via @racket[make-input] or else control will
not leave @racket[load-script].
}
