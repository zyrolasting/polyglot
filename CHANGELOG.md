# Changelog

This file summarizes changes to the project over time.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

Each version is in the format `major.minor`. `minor` and `major`
increments are subject to the author's interpretation of the words,
but generally mean that `major` changes have "bigger" impact than
`minor` changes. That said, _breaking changes have no relationship to
version numbers._ By policy, any breaking change is simply avoided
unless there's a good reason to introduce one. It's [too difficult to
draw a reliable relationship][jash], since some breaking changes are
meant to prepare intended functionality (e.g. fixing incorrect module
level contracts, see v1.6). The social contract is that you should not
upgrade until you have reason to do so, and if you are starting out
with this project, you should use the latest version.

## Unreleased
* Add missing version declaration in `info.rkt`.
* Copy README to `polyglot-lib` to fix `demo` command.
* Fixed return value bug in `run-txexpr/imperative!` for default (identity) layout
* Fixed port closure bug in `run-txexpr/imperative!`

## [2.8] - 2020-02-19
* Fixed broken `run-txexpr/functional!` contract.
* Fixed incorrect return value specs in `polyglot/txexpr` manual.
* Wrote `safe` submodules to move expensive contracts aside for default use.
* Divided source into several multi-collection packages.
* Added support for subdirectories in `publish` command.

## [2.7] - 2019-12-28
* Fix doc link in README
* Add missing links in docs to `polyglot/elements` and `racket/dict` bindings.
* Add `srcset` to dependency discovery
* Set `discover-dependencies` to respect lexical order of dependency attribute values
* Add missing `web-server-lib` dependency

## [2.6] - 2019-12-17
* Launch development server with `polyglot develop`.

## [2.5] - 2019-12-17
* Remove dead code
* Fix link to `unlike-assets-logger` in docs
* Fix `css-expr` dependency break

## [2.4] - 2019-12-16
* Add `make-temp-ephmod-directory` to `polyglot/paths`
* Add `polyglot/elements` and docs
* Change package description
* Delete dead code

## [2.3] - 2019-12-16
* Fix whitespace errors and typos
* Fix use of incorrect function in CSS handler
* Allow users to override asset and dist directories.
* Add tutorial for CSS preprocessing
* Make sure removed files are gone by the time a rebuild occurs.
* Make `--delay` actually do something, and document what it does.

## [2.2] - 2019-12-10
* Remove `raco` prefix in subcommand help table.
* Remove superfluous content in documentation
* Fix broken code links in imperative workflow documentation
* Add `polyglot-live?` parameter

## [2.1] - 2019-12-05
* Support file-to-file relative paths in distributions

## [2.0] - 2019-12-04
* Rewrite documentation into guide, reference, how-tos, and tutorials.
* Fix a unit test

## [1.11] - 2019-12-02
* Fix relative path writes in CSS `url()` expressions (#33, #34)
* Add FUNDING.yml for GitHub Sponsors
* Add `make-dist-path-string` to `polyglot/paths`
* Documentation improvements
    - Use the term "Polyglot" when referring to the project, and variants like `polyglot` only when it makes sense.
    - Rephrase several paragraphs for clarity
    - Fix broken links in code

## [1.10] - 2019-11-26
* Fix non-empty stderr for `entry.rkt` tests

## [1.9] - 2019-11-25
* Document use of multiple workflows
* Add `rewrite` formal to `apply-manifest`
* Resolve `..`, etc. in `asset-path?`
* Fix `.literal` link rewrite in aggregate projects

## [1.8] - 2019-11-25
* Publish `polyglot/builds`
* Edit motivation document
* Fix loading error in `get-workflow-class`

## [1.7] - 2019-11-25
* Expose `copy-polyglot-skeleton-project!`

## [1.6] - 2019-11-25
* Add motivation document
* Add `get-directory-name` method
* Add note for symlink support in Setup documentation
* Edit paragraphs for clarity
* Fix module contract for `get-workflow-class`

## [1.5] - 2019-11-20
* Support asset file entry point in CLI
* Add `.literal` support.
* Relax `polyglot/projects` to allow optional rcfiles*
* Default to thrown exception in thunk when rcfile is not found
* Fix `ensure-empty-distribution!` exception when dist directory does not exist
* Some refactoring
* Edit documentation to match

## [1.4] - 2019-11-20
* Add `ensure-empty-distribution!`
* Document `equal?` behavior for `polyglot-project%' objects

## [1.3] - 2019-11-20
* Add `polyglot` racket launcher
* Edit documentation for readability

## [1.2] - 2019-09-19
* Publish `polyglot/projects`

## [1.1] - 2019-11-12
* Switch CTA from personal donation link to business subscription page
* Add CSS support with dependency discovery
* Add documentation for hooking before and after builds.

## [1.0] - 2019-10-08
* Promote Racket module dependency processing to base workflow.
* Add reload support for runtime config.
* Expose functional workflow to clients with documentation.
* Edit documentation to reflect code movement
* Add some log events
* Reformat code

## [0.9] - 2019-10-07
* Upgrade workflows
  - Add base workflow
  - Add the functional workflow
  - Rename default workflow to the imperative workflow (It stays the default for backwards compatibility)
* Use plainer language in README
* Explicitly define provided bindings and contracts for `polyglot/paths`
* Move package-relative runtime path
* Add tests that build functional and imperative projects
* Add more shared procedures to CLI handlers
* Refactor for readability

## [0.8] - 2019-10-02
* Expose and document `polyglot/txexpr`
* Rename `expand-forest` to `interlace-txexprs`
* Use `error-display-handler` correctly so that its output appears in logs.

## [0.7] - 2019-09-30
* Support exception logging
* Fix critical bug where Markdown files formed circular dependencies.
* Document `-b`/`--by-module` as part of the default workflow.

## [0.6] - 2019-09-29
* Support `-b`/`--by-module` CLI option to allow use of new Polyglot class.

## [0.5] - 2019-09-29
* Support replacing one element with many using `data-macro`.

## [0.4] - 2019-09-29
* Add contract to `run-txexpr!` and allow more flexible applications of it.
* Add `polyglot-temp-directory`

## [0.3] - 2019-09-27
* Add `preprocess-txexprs` to default workflow
* Add support for `data-macro`
* Minor typos and test fixes

## [0.2] - 2019-09-20
* Use multi-page Scribble documentation
* Support `application/racket` as app element media type.
* Introduce custom workflows
* Fix tests and contracts

## [0.1] - 2019-09-19
* Add support for Racket module dependencies for advancing assets
* Expose paths API to clients.
* Add logging
* Add credit to https://goessner.net/articles/svg/fractals/index.html
* Normalize error handling in `raco` commands
* Add link to documentation in README
* Add `raco polyglot start` command
* Declare missing `aws` and `markdown` dependencies
* Fix typos and use plainer language
* Remove `rash` depenency from demo.
* Ripple all non-Markdown changes to dependent assets, not just CSS.

## [0.0] - 2019-08-24
* Start Racket package
* Add `raco polyglot` command with `develop`, `build`, `demo`, and `publish` subcommands.

[Unreleased]: https://github.com/zyrolasting/polyglot/compare/v2.8...HEAD
[2.8]: https://github.com/zyrolasting/polyglot/compare/v2.7...v2.8
[2.7]: https://github.com/zyrolasting/polyglot/compare/v2.6...v2.7
[2.6]: https://github.com/zyrolasting/polyglot/compare/v2.5...v2.6
[2.5]: https://github.com/zyrolasting/polyglot/compare/v2.4...v2.5
[2.4]: https://github.com/zyrolasting/polyglot/compare/v2.3...v2.4
[2.3]: https://github.com/zyrolasting/polyglot/compare/v2.2...v2.3
[2.2]: https://github.com/zyrolasting/polyglot/compare/v2.1...v2.2
[2.1]: https://github.com/zyrolasting/polyglot/compare/v2.0...v2.1
[2.0]: https://github.com/zyrolasting/polyglot/compare/v1.11...v2.0
[1.11]: https://github.com/zyrolasting/polyglot/compare/v1.10...v1.11
[1.10]: https://github.com/zyrolasting/polyglot/compare/v1.9...v1.10
[1.9]: https://github.com/zyrolasting/polyglot/compare/v1.8...v1.9
[1.8]: https://github.com/zyrolasting/polyglot/compare/v1.7...v1.8
[1.7]: https://github.com/zyrolasting/polyglot/compare/v1.6...v1.7
[1.6]: https://github.com/zyrolasting/polyglot/compare/v1.5...v1.6
[1.5]: https://github.com/zyrolasting/polyglot/compare/v1.4...v1.5
[1.4]: https://github.com/zyrolasting/polyglot/compare/v1.3...v1.4
[1.3]: https://github.com/zyrolasting/polyglot/compare/v1.2...v1.3
[1.2]: https://github.com/zyrolasting/polyglot/compare/v1.1...v1.2
[1.1]: https://github.com/zyrolasting/polyglot/compare/v1.0...v1.1
[1.0]: https://github.com/zyrolasting/polyglot/compare/v0.9...v1.0
[0.9]: https://github.com/zyrolasting/polyglot/compare/v0.8...v0.9
[0.8]: https://github.com/zyrolasting/polyglot/compare/v0.7...v0.8
[0.7]: https://github.com/zyrolasting/polyglot/compare/v0.6...v0.7
[0.6]: https://github.com/zyrolasting/polyglot/compare/v0.5...v0.6
[0.5]: https://github.com/zyrolasting/polyglot/compare/v0.4...v0.5
[0.4]: https://github.com/zyrolasting/polyglot/compare/v0.3...v0.4
[0.3]: https://github.com/zyrolasting/polyglot/compare/v0.2...v0.3
[0.2]: https://github.com/zyrolasting/polyglot/compare/v0.1...v0.2
[0.1]: https://github.com/zyrolasting/polyglot/compare/v0.0...v1.1
[0.0]: https://github.com/zyrolasting/polyglot/releases/tag/v0.0

[jash]: https://gist.github.com/jashkenas/cbd2b088e20279ae2c8e
