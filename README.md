Since Rama defines it's own dataflow language as a DSL in Clojure, linting it 
is a difficult problem. This repo defines the rules for how Rama code should be 
re-written such that clj-kondo can provide helpful warnings and errors as 
effectively as possible.

## Setting up 

The clj-kondo extensions for Rama are not currently bundled in the latest 
release of Rama. To get these working in your project, you'll first need 
to clone this repository:

```
git clone https://github.com/redplanetlabs/rama-clj-kondo.git
```

Then, you can either copy the contents of the `clj-kondo.exports` directory 
into your `.clj-kondo`, so that you have 

```
<project-root>
├── .clj-kondo
│   ├── com.rpl
│   │   └── rama
│   │       ├── ...
```

Or, you can add the root of the `rama-clj-kondo` project to your classpath 
in either your `deps.edn` or `project.clj` files, and run one of the following 
commands, depending on where you added this to your classpath. 

``` sh
clj-kondo --lint "$(clojure -Spath)" --copy-configs --skip-lint
```

``` sh
clj-kondo --lint "$(lein classpath)" --copy-configs --skip-lint
```

<!--
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
-->

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
