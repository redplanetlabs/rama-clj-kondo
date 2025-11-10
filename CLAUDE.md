# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

rama-clj-kondo provides clj-kondo linting extensions for Rama's dataflow DSL. Since Rama code uses a linear dataflow style rather than Clojure's lexical scoping, these hooks transform Rama code into equivalent nested `let` expressions that clj-kondo can analyze.

## Development Commands

### Running Tests
```bash
clojure -M:test
```

The project uses Kaocha as the test runner (configured in deps.edn).

### Installing Linting Rules
To import the rama-clj-kondo rules into your local clj-kondo config:
```bash
clj-kondo --lint "$(clojure -Spath)" --copy-configs --skip-lint
```

## Architecture

### Core Transformation Strategy

The linter transforms Rama's linear dataflow code into nested `let` expressions. For example:

```clojure
;; Rama code:
(identity 1 :> *x)
(identity 2 :> *y)
(+ *x *y)

;; Transforms to:
(let [*x (identity 1)]
  (let [*y (identity 2)]
    (+ *x *y)))
```

### Key Components

**rama_hooks.clj** - Core transformation logic with three main multimethod hierarchies:

1. `split-form` - Partitions branching constructs (`<<if`, `<<cond`, `<<switch`, `<<sources`, `<<subsource`) into distinct code paths for variable unification
2. `handle-form` - Transforms special Rama forms that define new syntax or have special scoping rules (`<<ramaop`, `batch<-`, `loop<-`, `+compound`, `java-macro!`, etc.)
3. `validate-form` - Enforces context-specific restrictions (e.g., no Java interop or Clojure special forms in dataflow code)

**transform-body** / **transform-form** - The main transformation pipeline that:
- Extracts emit tokens (`:>`, `:error>`, etc.) to identify variable bindings
- Separates new bindings from rebindings
- Handles variable unification across branches
- Nests subsequent forms inside generated `let` expressions

**transform-module-body** / **transform-module-form** - Similar transformation for module declarations, converting linear `declare-depot`, `declare-pstate` calls into nested `let` bindings

**extract-emits** - Splits expressions on emit keywords (`:>`), separating the operation from output variables and anchors

### Variable Unification

When branches define the same variables, the transformer creates unified bindings available after the branch:

```clojure
;; Rama code:
(<<if condition
  (op1 :> *x)
 (else>)
  (op2 :> *x))
(use *x)

;; Transforms to:
(let [*x nil
      _ (if condition
          (let [*x (op1)] {*x *x})
          (let [*x (op2)] {*x *x}))]
  (use *x))
```

### Context Tracking

The dynamic var `*context*` tracks whether code is in `:dataflow` or `:foreign-select` contexts to enforce Rama's restrictions on using Clojure special forms, Java interop, lambda functions, and keyword functions.

### Hook Registration

config.edn registers top-level transformation hooks for forms like `defmodule`, `?<-`, `batch<-`, etc. Nested Rama forms are handled internally by the multimethod dispatch in rama_hooks.clj rather than directly in the config.

## Testing Strategy

**rama_hooks_test.clj** contains comprehensive tests organized by:

- Basic transformation functionality
- Top-level forms (`defmodule`, `?<-`, etc.)
- Branching constructs (`<<if`, `<<cond`, `<<switch`, `<<sources`, `<<subsource`)
- Special forms (`<<ramaop`, `batch<-`, `loop<-`, anchors, etc.)
- Module code transformations
- Clojure and Java interop
- Real-world examples from Rama demo gallery
- Illegal context validation

Tests use helper functions to parse S-expressions, apply transformations, and strip the inserted `trampoline` calls before assertion.

## Known Limitations

See README.md "Roadmap" section for features that don't currently lint correctly:
- Capturing emits from multiple output streams with anchors
- Defining anchors in nested contexts
- Anchors combined with output stream captures
