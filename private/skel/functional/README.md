This is an example that uses every `polyglot` feature.

When in this directory...

* `raco polyglot build .` builds the website once.
* `raco polyglot develop .` watches changes in `assets` and rebuilds when you save a file.
* `raco polyglot publish . BUCKET REGION` publishes the contents of `dist` to the given S3 bucket. [See documentation](https://docs.racket-lang.org/polyglot/index.html) for assumptions!
