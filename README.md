Since Rama defines its own dataflow language as a DSL in Clojure, linting it 
is a difficult problem. This repo defines the rules for how Rama code should be 
re-written such that clj-kondo can provide helpful warnings and errors as 
effectively as possible.

## Setting up 

The clj-kondo extensions for Rama come bundled with the Rama jar when 
installing dependencies. This means your editor should import the clj-kondo 
rules for you automatically, but in case it doesn't or you favour using 
clj-kondo from a terminal, you can get clj-kondo to import the linting rules 
for Rama by running 

You can get clj-kondo to import the linting rules for Rama by running:

``` sh
clj-kondo --lint "$(clojure -Spath)" --copy-configs --skip-lint
```

or

``` sh
clj-kondo --lint "$(lein classpath)" --copy-configs --skip-lint
```

If you're using leiningen.

Note that if your dependency on Rama is specified under a specific alias or
profile, you need to make sure to include that in the `clojure` or `lein`
command. Otherwise the Rama jar won't be on the classpath.

## Optional Linters

### Unverifiable PState references (`:rama-unverifiable-pobject`)

When `$$`-prefixed PState symbols or depot vars are used in standalone
`<<query-topology` or `<<sources` forms (outside a `defmodule`), the linter
cannot verify that they were declared in a module scope. This linter emits
info-level diagnostics for such references.

This linter is **disabled by default** because these references are typically
valid — they refer to PStates or depots declared elsewhere in the module. To
enable it, add the following to your project's `.clj-kondo/config.edn`:

``` clojure
{:linters {:rama-unverifiable-pobject {:level :info}}}
```

You can also set the level to `:warning` or `:error` if desired.

## Roadmap

There are a number of Rama features that are known to not lint correctly. 
Since Rama segments get transformed into a graph, reorganizing the code to 
successfully lint everything is difficult, and as such skips out on a lot of 
that at the moment.

- Capturing emits from multiple output streams 

``` sh
(my-ramaop :> *out :error> *ex)
```

- Defining anchors in a nested context

``` sh
(anchor> <X>)
(<<branch <X>
  (anchor> <Y>))
(<<branch <Y>)
```

- Defining anchors as a part of capturing output streams 

``` sh
(my-ramaop :> *out :error> <error> *ex)

(<<branch <error> 
  (println *ex))
```

## How it works

Rama dataflow code is written in, what appears to be, a linear style, compared 
to Clojure where everything is lexically scoped. As such, the general premise 
of this is to transform Rama code so that emits are rewritten as nested `lets`.

For example, 

``` sh
(?<- 
  (identity 1 :> *x)
  (identity 2 :> *y)
  (println (+ *x *y)))
```
Would be rewritten as 
``` sh
(let [*x (identity 1)]
  (let [*y (identity 2)]
    (println (+ *x *y))))
```

The tests contain extensive examples of how every type of form is rewritten 
such that it can be interpreted as regular Clojure code.

Since having access to the following forms is important for the transformation 
rules, hooks in the `config.edn` are only defined for top-level forms. 
Transformation rules for special Rama forms, such as `<<if` or `batch<-` are 
written as extensions of the `split-form` or `handle-form` methods in 
`rama_hooks.clj`.

## Development

### Git Hooks

This project includes scripts to install Git hooks for maintaining code quality:

**Pre-commit hook** - Automatically formats `.clj` and `.edn` files using cljfmt when you commit:

``` sh
./scripts/install-pre-commit-hook.sh
```

**Pre-push hook** - Runs clj-kondo linting before pushing to prevent pushing code with warnings or errors:

``` sh
./scripts/install-pre-push-hook.sh
```

The pre-push hook will block pushes if clj-kondo finds any issues in the project. 

### Regression lint tests on external projects

Run external-project regression tests (separate from the unit test suite) with:

``` sh
clojure -M:test-regression
```

These tests clone pinned commits of external Rama projects into `test-regression/checkouts/`, lint them with this repo's hooks, and compare the findings to checked-in fixtures.

Regression targets are declared in `test-regression/com/rpl/rama_hooks/projects.edn`.

To update fixtures after intentional linting changes:

``` sh
CLJ_KONDO_REGRESSION_UPDATE=1 clojure -M:test-regression
```

### Babashka tasks

For convenience, the same workflows are available via `bb`:

``` sh
bb test
bb regression
bb regression:update
bb snapshots:update
bb regression:targets
bb regression:findings
bb check
```

- `bb test` – run unit tests
- `bb regression` – run external-project regression lint tests
- `bb regression:update` – regenerate expected findings fixtures
- `bb snapshots:update` – alias of `bb regression:update`
- `bb regression:targets` – list each regression target repo/sha and its findings fixture file
- `bb regression:findings` – print tolerated findings counts per target
- `bb check` – run cljfmt check + unit tests + regression tests

