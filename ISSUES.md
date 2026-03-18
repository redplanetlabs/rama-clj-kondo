# Known Issues

Remaining false positives and limitations identified via the rama-tpcc
regression suite (20 tolerated findings, down from 150).

## Mitigated Issues

### Cross-function pstate/depot scope (8 findings → info)

Pstates and depots declared in a helper function are not visible in
other helper functions called from the same module. The transformer
only tracks bindings within a single function scope.

These are now reported as info-level `:rama-unverifiable-pobject`
diagnostics instead of false-positive unresolved-symbol errors.
The linter detects `$$`-prefixed pstate refs and `*`-prefixed depot
vars (via a registry of names seen in `declare-depot`/`declare-tick-depot`
calls) that cannot be verified as locally bound.

**Limitation:** depot name detection across files relies on clj-kondo
processing the declaring file before the referencing file. Within a
single file this is guaranteed; across files it depends on lint order.

**Findings (now info):** tpcc.clj:641 $$customer-by-last,
tpcc.clj:648 $$customer, tpcc.clj:653 $$order, tpcc.clj:671 $$district,
tpcc.clj:682 $$stock, tpcc.clj:689 *transaction-depot,
tpcc.clj:692 *cleanup-tick, tpcc.clj:721 $$processed-items

## True Positives in tpcc Code

These are genuine issues in the tpcc source, not linter bugs.

### Unused bindings (5 findings)

- tpcc.clj:340 `*w-tax` — bound via destructuring, never referenced
- tpcc.clj:341 `*d-tax` — bound via destructuring, never referenced
- tpcc.clj:398 `*o-id` — destructured in loop body, unused before continue>
- tpcc.clj:441 `*final-total` — bound via destructuring, never referenced
- load_runner.clj:249 `item-result` — bound but never referenced

### Misplaced docstrings (4 findings)

- tpcc.clj:40, tpcc.clj:61 — string literals in function body position
  (misplaced-docstring + unused-value for each)

### Java import issues (4 findings)

- load_runner.clj:12 — `AtomicBoolean` and `AtomicInteger` reported as
  unused imports AND unresolved symbols at line 146, suggesting the
  `:import` form isn't being recognized correctly by clj-kondo

### Test file issues (5 findings)

- tpcc_test.clj:2 — `:use` instead of `:require`
- tpcc_test.clj:5,6 — unused namespace requires (rama.aggs, rama.ops)
- tpcc_test.clj:389 — unused binding `cust-by-last-ps`
- tpcc_test.clj:518, tpcc_test.clj:1109 — redundant let expressions

### Redundant let (1 finding)

- load_runner.clj:517 — redundant let expression

## Future Improvements

### Replace unresolved-symbol exclude list with namespace stubs

The current config.edn maintains a manual list of ~170 excluded symbols
for `com.rpl.rama` and `com.rpl.rama.path`. This suppresses all
unresolved-symbol errors for those names globally, meaning typos in
Rama symbol names are silently accepted.

The preferred approach is to generate namespace stub files (using
[clj-easy/stub](https://github.com/clj-easy/stub)) that declare all
public vars. This would let clj-kondo resolve `:refer :all` / `:use`
properly and catch actual typos.
