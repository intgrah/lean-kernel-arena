import VersoBlog

open Verso Doc Verso.Doc.Concrete Verso.Genre.Blog


namespace ArenaSite.Descriptions.Tests

def «bogus1» : VersoDoc Page :=
  verso (Page) "bogus1"
  :::
  A clearly bogus proof. Also serves as an example for how to write simple cases.
  :::

def «cedar» : VersoDoc Page :=
  verso (Page) "cedar"
  :::
  Lean formalization of, and proofs about, [Cedar](https://www.cedarpolicy.com/).

  Auto-generated documentation is available at https://cedar-policy.github.io/cedar-spec/docs/.

  This test case exports the whole `Cedar` module and as such contains even unused parts `Init` and
  `Batteries.`
  :::

def «constlevels» : VersoDoc Page :=
  verso (Page) "constlevels"
  :::
  Regression test for undefined behavior in `lazy_delta_reduction_step` in the official kernel

  In the function `lazy_delta_reduction_step`, the official kernel expects `unfold_definition`
  to always succeed. However, if the constant has an incorrect number of level parameters, it
  actually fails, which leads to memory corruption in `lazy_delta_reduction_step`.

  This test is to check that the official kernel and also other kernels that closely follow the
  logic of the official kernel correctly handle this unfolding failure.

  The issue in the official kernel was originally reported as [https://github.com/leanprover/lean4/issues/10577](https://github.com/leanprover/lean4/issues/10577).
  :::

def «cslib» : VersoDoc Page :=
  verso (Page) "cslib"
  :::
  The Lean Computer Science Library (CSLib). 
  :::

def «ctor-num-fields» : VersoDoc Page :=
  verso (Page) "ctor-num-fields"
  :::
  Proof of False via trusted `numFields` on a constructor.

  Define a wrapper structure `S` with one field, and lie by saying it has 0 fields, making
  it look unit-like. Then definitional eta means all inhabitants are equal.

  Derive a contradiction from `S.mk false = S.mk true`.
  :::

def «init» : VersoDoc Page :=
  verso (Page) "init"
  :::
  The `Init` module export from Lean 4 core.

  This test contains the fundamental building blocks of Lean 4, including:

  * Basic data types (Nat, List, Array, String, etc.)
  * Core tactics and syntax
  * Foundational mathematical structures
  * Essential metaprogramming infrastructure

  This is one of the smallest meaningful test cases, making it ideal for 
  initial checker validation and debugging.
  :::

def «init-prelude» : VersoDoc Page :=
  verso (Page) "init-prelude"
  :::
  The `Init.Prelude` module export.
  :::

def «k-rec-conv» : VersoDoc Page :=
  verso (Page) "k-rec-conv"
  :::
  Bogus proof that tests for incorrectly implemented K-like reduction.

  `fun x => x` and `fun _ => y` are not convertible, but a checker that does treat them
  as convertible would accept the resulting theorem `bad`, which is true propositionally, but not definitionally.

  Regression test for sokonanoda.
  :::

def «large-elim-param» : VersoDoc Page :=
  verso (Page) "large-elim-param"
  :::
  Proof of False via incorrect large elimination restriction.

  If the check for whether a level is surely not zero is implemented wrong, in
  particular if it incorrectly returns true for params, we can create a
  universe-polymorphic
  ```
  inductive MyBool.{u} : Sort u | tt | ff
  ```
  where the recursor `MyBool.rec.{1,0}` can do large elimination of a `Prop`.
  Because of proof irrelevance we have `tt = ff`, so we can derive a
  contradiction.

  Found by Anthony Wang using Aristotle.
  :::

def «level-imax-leq» : VersoDoc Page :=
  verso (Page) "level-imax-leq"
  :::
  Proof of False via incorrect universe level comparison for `imax`.

  A correct kernel must reject `leq(imax(u,v)+1, imax(u,v))`, since at `u=0, v=0` this becomes
  `leq(1, 0)` which is false. However, a checker that only compares the `imax` arguments
  structurally (without accounting for an accumulated successor offset) will incorrectly accept it.

  This allows defining a universe-collapsing identity function
  `down.{u,v} : Sort (succ (imax u v)) → Sort (imax u v)`, which is used to cast between
  `True` and `False` via `Bool.rec` at `Sort (imax 0 0) = Prop`.

  Nanoda incorrectly accepted this proof until it was [fixed](https://github.com/ammkrn/nanoda_lib/commit/12838995ca232ced05cd5218e4975d03c5c44316).
  :::

def «level-imax-normalization» : VersoDoc Page :=
  verso (Page) "level-imax-normalization"
  :::
  Proof of False via incorrect universe level normalization for `imax`.

  A correct kernel must distinguish `imax 0 v` from `succ(imax 0 v)`, since at `v=0` these
  evaluate to `0` and `1` respectively. However, a level normalization algorithm that drops an
  accumulated successor offset when decomposing `imax u (param v)` will produce identical normal
  forms for both, causing the equivalence check to incorrectly return true.

  This allows defining a universe-collapsing identity function
  `down.{v} : Sort (succ (imax 0 v)) → Sort (imax 0 v)`, and then
  `myProp : Prop := down.{0} Bool` (a Prop that is computationally Bool). Proof irrelevance on
  `myProp` equates `Bool.true` and `Bool.false`, and `Bool.rec` maps this into `False`.
  :::

def «level-index-out-of-order» : VersoDoc Page :=
  verso (Page) "level-index-out-of-order"
  :::
  Lean4export will create internalization-table references contiguously in
  order: `in` references for names, `il` references for levels, and `ie`
  references for expressions all work this way.

  However, the spec merely requires that these are integers. It's reasonable
  for an implementation to assume these are approximately dense (and to treat
  them as array indices instead of hashtable entries), but a kernel should
  handle skipped indices or out-of-order indices.

  This test checks that the kernel doesn't require internaliation-table
  references to be presented in ascending order. If the level referenes 2 and
  1 were swapped, this would be the expected encoding of `axiom foo : Sort 2`.
  This encoding should be equivalent.
  :::

def «mathlib» : VersoDoc Page :=
  verso (Page) "mathlib"
  :::
  The complete [Mathlib](https://github.com/leanprover-community/mathlib4) library export.

  This test contains all the mathematical definitions, theorems, and proofs
  from Mathlib, representing the largest and most comprehensive test case
  in the Lean kernel arena.
  :::

def «nat-rec-k-lie» : VersoDoc Page :=
  verso (Page) "nat-rec-k-lie"
  :::
  Proof of False via trusted `k` on `Nat.rec`.

  Lie by claiming `Nat.rec` is K-like. Then replace the major premise by `Nat.zero`,
  but nat literals bypasses K-like reduction, so two reduction rules disagree.

  `∀ n, g n` holds by the first, and `g 1` is `False` by the second.
  :::

def «nat-rec-rules» : VersoDoc Page :=
  verso (Page) "nat-rec-rules"
  :::
  Proof of False via incorrect recursor rule validation.

  When processing an inductive type declaration, a correct kernel must verify that the generated
  recursor rules match the ones provided in the export data. A checker that accidentally compares
  the imported rules against themselves (instead of against independently constructed rules) will
  accept arbitrary recursor reduction behavior.

  This test defines `Nat` with a wrong `Nat.rec` succ rule that always returns `hzero` (ignoring
  the induction hypothesis). Combined with a nat literal extension that hardcodes correct
  arithmetic for concrete nat literals but falls back to the wrong `Nat.rec` rules for symbolic
  arguments, this creates an inconsistency that yields a proof of False.

  Nanoda incorrectly accepted this proof until it was [fixed](https://github.com/ammkrn/nanoda_lib/commit/12838995ca232ced05cd5218e4975d03c5c44316).
  :::

def «perf/app-lam» : VersoDoc Page :=
  verso (Page) "perf/app-lam"
  :::
  A synthetically generated term with **n levels of alternating applications
  and lambdas**, with **DAG sharing**.

  At each level, a constant is applied to two identical lambda arguments. The
  export format records these as a single shared expression (DAG). Each lambda
  body grows with the nesting depth, referencing all enclosing binders.

  This tests two aspects of checker performance:

  **Infer cache**: Since both arguments at each level are the same expression,
  a checker without an infer cache re-infers the type of each shared subterm,
  doubling work at every level — **O(2ⁿ) total**.

  **Substitution cost**: Even with a cache, type-inferring each lambda requires
  substituting into its body (size O(n)) at each of the n levels, giving
  **O(n²) total**. Whether this cost arises depends on the checker's binder
  representation.
  :::

def «perf/grind-ring-5» : VersoDoc Page :=
  verso (Page) "perf/grind-ring-5"
  :::
  A **grind tactic test** from the Lean 4 test suite.

  This produces a theorem with a rather large proof term that needs fast reduction.
  :::

def «perf/shift-cascade» : VersoDoc Page :=
  verso (Page) "perf/shift-cascade"
  :::
  Stress test for cascading substitution overhead in kernel `let` processing.

  N nested `let` bindings inside a lambda, where each value references the
  outer lambda parameter and the previous binding:

    fun (a : Nat → Nat) =>
      let f₁ := fun x => a x
      let f₂ := fun x => a (f₁ x)
      ...
      let fₙ := fun x => a (fₙ₋₁ x)
      fₙ 0

  The kernel processes each `let` by substituting the value into the body.
  Each value has a free bvar (references `a`), so substitution under inner
  binders creates shifted copies. In a de Bruijn kernel with deferred shifts,
  these `Shift(val, offset)` wrappers accumulate: step k must traverse
  through O(k) wrappers from previous steps, giving **O(N²) total** work.

  A locally-nameless kernel substitutes fvars that need no shifting,
  giving **O(N) total**.

  N=1000 in the Lean source. Increase to stress further.
  :::

def «proj-non-structure» : VersoDoc Page :=
  verso (Page) "proj-non-structure"
  :::
  `Bad` has two constructors, so projections should not be allowed.
  Prove false by using the second constructor, then projecting, hoping that the
  first constructor is used when inferring the type of the projection.
  :::

def «proj-of-prop» : VersoDoc Page :=
  verso (Page) "proj-of-prop"
  :::
  A proof of `False` via a projection from a `Prop`-typed structure whose
  constructor was applied to an ill-typed argument. The exported term is

      badFalse : False := (Wrapper.mk True.intro).p

  where `Wrapper : Prop` has a single field `p : False`, so `Wrapper.mk`
  expects a proof of `False` but is given `True.intro : True`.

  A sound checker must reject this. A checker that types a projection by
  inferring (rather than checking) its structure argument — i.e. that
  trusts the structure to be well-typed instead of verifying the
  constructor's argument types against its binders — will accept it,
  because `Wrapper.mk True.intro` still formally inhabits `Wrapper` at
  the structural level, and the `p` projection is then read back out at
  the declared field type `False`.
  :::

def «proof-irrel» : VersoDoc Page :=
  verso (Page) "proof-irrel"
  :::
  Incompleteness test for proof irrelevance under a binder.

  `bar : ∀ h : A → P, Q (h a) := foo` where `foo : ∀ h : A → P, Q (h b)`.
  Checking the assignment needs `Q (h a) ≡ Q (h b)`, i.e. `h a ≡ h b`. Both
  `h a` and `h b` are proofs of the same `Prop` `P`, so they are definitionally
  equal by proof irrelevance and a complete kernel accepts.

  A checker that fails to apply proof irrelevance here — comparing `h a` and
  `h b` structurally and finding the arguments `a` and `b` distinct — wrongly
  rejects a valid proof.
  :::

def «rec-k-lie» : VersoDoc Page :=
  verso (Page) "rec-k-lie"
  :::
  False theorem via trusted `k` on a recursor.

  Define `MyBool` with two constructors, and lie by claiming its recursor is K-like,
  so the major premise is replaced by the first constructor without being examined.

  `disc MyBool.true` is then `True` rather than `False`.

  `MyBool` rather than `Bool` because a module that overwrites an imported constant
  cannot be re-imported by the exporter.
  :::

def «sparse-name-index» : VersoDoc Page :=
  verso (Page) "sparse-name-index"
  :::
  Lean4export will create internalization-table references contiguously in
  order: `in` references for names, `il` references for levels, and `ie`
  references for expressions all work this way.

  However, the spec merely requires that these are integers. It's reasonable
  for an implementation to assume these are approximately dense (and to treat
  them as array indices instead of hashtable entries), but a kernel should
  handle skipped indices or out-of-order indices.

  This test checks that a kernel doesn't require internalization-table
  references to be assigned sequentially starting from 1. If the "2" and "4" 
  were replaced by "1" and "0", respectively, this would be the expected 
  encoding of `axiom foo : Prop`. This encoding should be equivalent.
  :::

def «std» : VersoDoc Page :=
  verso (Page) "std"
  :::
  The complete `Std` library export from Lean 4.

  This test contains the standard library extensions beyond core Lean 4,
  including:

  * Enhanced data structures (HashMap, RBTree, etc.)
  * Additional mathematical operations
  * Extended list and array operations
  * Utility functions and theorems

  This represents a medium-sized test case, larger than core modules
  but smaller than Mathlib, making it useful for performance testing.
  :::

def «tutorial» : VersoDoc Page :=
  verso (Page) "tutorial"
  :::
  Multiple tutorial tests generated from a Lean project in the tutorial directory.
  :::

def «Undecidability/alg-conv-trans-acc-left» : VersoDoc Page :=
  verso (Page) "Undecidability/alg-conv-trans-acc-left"
  :::
  The creative half of `undecidability/alg-conv-trans-acc`. `Acc.rec` is stuck
  on the variable `a`, and proof irrelevance admits any other proof of
  `Acc (· < ·) 1` in its place, including one with a constructor at the head.
  Given both sides, a checker verifies this immediately; producing the
  right-hand side unprompted is the step no algorithm takes.
  :::

def «Undecidability/alg-conv-trans-acc-right» : VersoDoc Page :=
  verso (Page) "Undecidability/alg-conv-trans-acc-right"
  :::
  The mechanical half of `undecidability/alg-conv-trans-acc`. With a
  constructor in the major premise, `Acc.rec` fires and `step` descends to the
  predecessor `0`.
  :::

def «Undecidability/alg-conv-trans-acc» : VersoDoc Page :=
  verso (Page) "Undecidability/alg-conv-trans-acc"
  :::
  As Lean's type theory has undecidable conversion (a.k.a. definitional equality),
  there are bound to be gaps between so called "algorithmic" conversion (that which is
  implemented by a typechecker), and the "declarative" conversion.

  In the official kernel, algorithmic conversion fails to be transitive. `f 1 a`
  is a normal form: `a` is a variable, so `Acc.rec` cannot fire on it. Proof
  irrelevance admits any other proof of `Acc (· < ·) 1` in its place, and
  `Acc.intro 1 fun _ => Acc.inv a` carries a constructor at the head, so it
  reduces. `left` is that substitution, `right` the reduction it unblocks, and
  `trans` chains the two.

  `acc` asks for the endpoints on their own, which means inventing the middle
  term: choosing, among the proofs of a proposition, the one that happens to
  reduce the right way. The kernel has no reason to go looking, the left side
  being normal already, and unfolding regardless does not terminate here, as
  each step makes the term larger.

  References:

  - Mario Carneiro, *The Type Theory of Lean*, MSc thesis
  :::

def «Undecidability/alg-conv-trans-quot-left-def» : VersoDoc Page :=
  verso (Page) "Undecidability/alg-conv-trans-quot-left-def"
  :::
  `left` with `Quot.lift` behind a definition. WHNF does not unfold `lift`, so
  the arguments are compared and proof irrelevance applies.
  :::

def «Undecidability/alg-conv-trans-quot-left» : VersoDoc Page :=
  verso (Page) "Undecidability/alg-conv-trans-quot-left"
  :::
  `Quot r` is a `Prop`, so proof irrelevance relates `q` and `Quot.mk r z`.
  However the official kernel does WHNF first, reducing the right side to `f z`,
  so congruence never compares the arguments.
  :::

def «Undecidability/alg-conv-trans-quot-right» : VersoDoc Page :=
  verso (Page) "Undecidability/alg-conv-trans-quot-right"
  :::
  Quotient computation rule.
  :::

def «Undecidability/alg-conv-trans-quot» : VersoDoc Page :=
  verso (Page) "Undecidability/alg-conv-trans-quot"
  :::
  `left` composed with `right`. Quotients of propositions cause
  algorithmic conversion transitivity to fail because the typechecker must
  creatively synthesise the representative of the quotient, and
  proof irrelevance is definitional.

  References:

  - Mario Carneiro, *The Type Theory of Lean*, MSc thesis
  :::

def «Undecidability/subject-reduction-redex» : VersoDoc Page :=
  verso (Page) "Undecidability/subject-reduction-redex"
  :::
  Test for subject reduction, as in Carneiro's thesis.

  The annotation on the lambda writes the middle term of
  `undecidability/alg-conv-trans-acc` down by hand, sparing the kernel from
  having to invent it. The body checks against `right`, the argument against
  `left`, and the two endpoints are never compared.

  References:

  - Mario Carneiro, *The Type Theory of Lean*, MSc thesis
  :::

def «Undecidability/subject-reduction-reduct» : VersoDoc Page :=
  verso (Page) "Undecidability/subject-reduction-reduct"
  :::
  Beta erases the annotation of `undecidability/subject-reduction-redex`, and
  with it the middle term, leaving the two endpoints to compare: the conversion
  of `undecidability/alg-conv-trans-acc`. A term the kernel accepts thus
  reduces to one it rejects.

  References:

  - Mario Carneiro, *The Type Theory of Lean*, MSc thesis
  :::

def «nested-nonuniform-param» : VersoDoc Page :=
  verso (Page) "nested-nonuniform-param"
  :::
  Checks that a parameter supplied to a nested inductive occurrence really acts
  as the datatype's parameter, i.e. that it is the parameter itself and does not
  change between recursive occurrences (as is already enforced for non-nested
  occurrences).

  The inductive `E : W → Type` has constructor
  `E.mk : (w : W) → L (E ⟨false⟩) → E w`, where `L (α : Type)` is nested. The
  occurrence `E ⟨false⟩` inside the nested `L` uses the constant `⟨false⟩` in
  the position of `E`'s parameter, instead of the actual parameter `w`. That
  argument is type-correct, so it is not caught by merely type-checking the
  nested application (leanprover/lean4#14577); a correct checker must also
  verify that it is the expected parameter.

  This particular declaration is not known to yield a proof of `False`: here `L`
  stores no value of type `α`, so the nested occurrence is phantom and `E w` is
  isomorphic to `Unit` for every `w`. The variant where `L` actually stores an
  `α` (so recursion would descend into an `E ⟨false⟩` while the motive is fixed
  at `E w`) is already rejected by the kernel's positivity check ("non valid
  occurrence"). Since it is not a demonstrated unsoundness, it is not settled
  whether a checker should accept or reject it, so the expected outcome is
  `either` and the test does not count towards completeness or soundness.

  Origin: raised by @arthur-adjedj on leanprover/lean4#14577
  (https://github.com/leanprover/lean4/pull/14577#issuecomment-5101819377) as a
  case not covered by that PR's fix; related to leanprover/lean4#14576.
  :::

def «nested-unused-param» : VersoDoc Page :=
  verso (Page) "nested-unused-param"
  :::
  Checks that the parameters of a nested inductive application are type-checked
  even when they do not appear in the auxiliary type generated during
  nested-inductive compilation.

  When an inductive `E` has a constructor whose type contains a nested
  application `L (E w) b`, the elaboration of nested inductives replaces that
  occurrence with an auxiliary type. The argument `b` does not occur in the
  auxiliary declaration, so a checker that only checks the auxiliary type never
  sees `b`. A correct checker must still ensure `b` is well-typed; this test
  rejects if it is not.

  Here `b` is a malformed projection `C.0 (C.0 w)` (applying a `C` projection to
  a value of the unrelated structure `W`), disguised by a hash collision. If the
  parameter is not checked, the bogus projection slips through and the resulting
  `E` can be used to build an axiom-free proof of `False` (`boom`). The
  projection is merely the payload; the property under test is that the
  nested-inductive parameter is checked.

  Origin: reported as leanprover/lean4#14576 by @kiranandcode, with the original
  source recorded by @xrchz (https://github.com/xrchz/collatzlean); fixed in
  leanprover/lean4#14577.
  :::

end ArenaSite.Descriptions.Tests

namespace ArenaSite.Descriptions.Checkers

def «always-accept» : VersoDoc Page :=
  verso (Page) "always-accept"
  :::
  A **trivial checker** that always accepts any input.

  This checker simply runs `exit 0` and is useful for:

  * Testing the framework
  * Establishing baseline performance metrics
  * Debugging test generation issues
  :::

def «always-decline» : VersoDoc Page :=
  verso (Page) "always-decline"
  :::
  A **trivial checker** that always declines any input.

  This checker simply runs `exit 2` and is useful for:

  * Testing framework handling of declined tests
  * Measuring parsing and setup overhead
  * Establishing baseline timing for non-checking operations

  Exit code 2 indicates the checker declined to process the input,
  different from rejection (exit 1) which indicates an invalid proof.
  :::

def «always-reject» : VersoDoc Page :=
  verso (Page) "always-reject"
  :::
  A **trivial checker** that always rejects any input.

  This checker simply runs `exit 1` and is useful for:

  * Testing framework handling of rejected proofs
  * Validating error reporting and status tracking
  * Establishing baseline performance for failed checks

  Exit code 1 indicates the checker found the proof invalid,
  simulating what would happen with malformed or incorrect proofs.
  :::

def «evmlean» : VersoDoc Page :=
  verso (Page) "evmlean"
  :::
  A Lean 4 kernel implemented in Solidity and executed on the Ethereum
  Virtual Machine. Accepting a proof is a metered EVM execution; the same
  call can be replayed on any EVM chain (36.7KB deployed — sized for the
  EIP-7907 limit). Rejects all five of the Arena's static adversarial
  exports at the offending declaration. Declines declarations outside its
  fragment (nested inductives, multi-type mutual groups, unsafe decls,
  String reduction) and exports beyond its resource budget.
  :::

def «lean4lean» : VersoDoc Page :=
  verso (Page) "lean4lean"
  :::
  [**Lean4Lean**](https://github.com/digama0/lean4lean/) is an implementation of the Lean 4 kernel written in (mostly) pure Lean 4. It is derived directly from the C++ kernel implementation, and as such likely shares some implementation bugs with it (it's not really an independent implementation), although it also benefits from the same algorithmic performance improvements existing in the C++ Lean kernel.

  The project also houses some metatheory regarding the Lean system, in the same general direction as the MetaCoq project.

  The lean4lean checker checks that primitive definitions from the prelude are as expected. For that reason, it is tied to a specific Lean version. The arena uses a wrapper script to switch to the appropriate version for the tests we have here.
  :::

def «mini» : VersoDoc Page :=
  verso (Page) "mini"
  :::
  A small, naive and incomplete implementation of a Lean kernel from scratch.

  This is mostly an exercise to improve the test coverage on the arena.
  Runs with a 30s timeout.
  :::

def «nanobruijn» : VersoDoc Page :=
  verso (Page) "nanobruijn"
  :::
  **Nanobruijn** - An experimental Lean 4 kernel using pure de Bruijn indices.

  Forked from [nanoda\_lib](https://github.com/ammkrn/nanoda_lib), this variant
  replaces the locally-nameless binding representation with pure de Bruijn
  indices. Written in Rust.

  Key ideas:

  * **Pure de Bruijn indices** instead of locally-nameless: entering a binder
    requires no substitution — variables are shifted lazily when read from
    the local context.
  * **Lazy shifting via pointer+offset pairs**: instead of allocating shifted
    expression nodes, each pointer carries an integer offset. Shifting is O(1)
    "pointer arithmetic". Expressions that differ only by a uniform index shift
    share a single DAG node.
  * **Depth-stratified caching**: cache entries are bucketed by binding depth,
    so results at one depth can be reused at another by adjusting the offset.
    Closed (variable-free) expressions live in a global bucket that survives
    all context changes.
  * **Lazy frame invalidation**: when leaving a binder, the cache frame is
    kept rather than discarded. If the next binder entry has the same type,
    the frame (with all its cached results) is reused.

  This is an experimental checker, not intended for production use. All code
  modifications from the original nanoda\_lib were written by Claude (Anthropic).
  :::

def «nanoda» : VersoDoc Page :=
  verso (Page) "nanoda"
  :::
  **Nanoda** - An alternative Lean 4 kernel implementation in Rust.

  This is an independent proof checker that validates Lean 4 exports
  using a different implementation approach. Features:

  * Written in Rust for performance
  * Supports standard Lean 4 axioms (propext, choice, etc.)
  * Configurable axiom checking policy
  * Native JSON parsing with string optimizations
  * Parallel checking using multiple threads (set to 4 threads in the kernel arena)

  Useful for cross-validation against the official kernel and
  testing implementation diversity in the Lean ecosystem.

  The nanoda integration in the arena does not distinguish between a rejected proof and other forms
  of failure, so they are all repoted as “rejected”.
  :::

def «nyaya» : VersoDoc Page :=
  verso (Page) "nyaya"
  :::
  nyaya -- an external Lean 4 kernel written in OCaml. 
  :::

def «official» : VersoDoc Page :=
  verso (Page) "official"
  :::
  The official Lean 4 kernel checker implementation.

  This is the reference implementation from the Lean 4 repository,
  wrapped in a program that reads the exported proofs and replays them in the kernel.
  :::

def «official-nightly» : VersoDoc Page :=
  verso (Page) "official-nightly"
  :::
  The official Lean 4 kernel checker implementation, nightly release.
  :::

def «official-v4.28.0» : VersoDoc Page :=
  verso (Page) "official-v4.28.0"
  :::
  The official Lean 4 kernel checker implementation (v4.28.0).
  :::

def «parse-only» : VersoDoc Page :=
  verso (Page) "parse-only"
  :::
  The official Lean 4 kernel in **parse-only mode**.

  This checker only validates that the exported data can be parsed correctly
  without performing full kernel checking. It's useful for:

  * Testing export format compatibility
  * Measuring parsing performance separately from verification
  * Debugging export data corruption issues

  Uses the same official kernel as the main checker but with `--parse-only` flag.
  :::

def «rpylean» : VersoDoc Page :=
  verso (Page) "rpylean"
  :::
  **rpylean** - A Lean type checker written in (R)Python.
  :::

def «sokonanoda» : VersoDoc Page :=
  verso (Page) "sokonanoda"
  :::
  **sokonanoda** — a Lean 4 kernel whose conversion checker is
  based on normalization by evaluation (NbE).

  This is a fork of
  [still-nanoda](https://github.com/SchrodingerZhu/still-nanoda).
  The conversion algorithm uses "glued" evaluation, as in András Kovács'
  [smalltt](https://github.com/AndrasKovacs/smalltt).
  :::

def «still-nanoda» : VersoDoc Page :=
  verso (Page) "still-nanoda"
  :::
  **Still-nanoda** - A fork of nanoda/sonanoda with experiemental optimization efforts.
  :::

def «vow-lean-kernel» : VersoDoc Page :=
  verso (Page) "vow-lean-kernel"
  :::
  A Lean 4 kernel (proof checker) written in [**Vow**](https://github.com/vow-lang/vow),
  a young systems language. Reads `lean4export` NDJSON and verifies each
  declaration is well-typed.

  It accepts the full Arena tutorial suite and all of Lean core `Init`
  (54,475 declarations). Nat arithmetic uses a base-2^32 bignum backend, so
  reductions past 2^64 are exact. Declarations beyond the currently supported
  fragment are **declined** (exit 2) rather than falsely rejected — the checker
  is sound-by-construction on what it accepts and conservative elsewhere.

  [Vow](https://vow-lang.com) is a language developed by Paulo Matos
  ([github.com/pmatos](https://github.com/pmatos)) for agentic use — one of
  several such languages currently emerging, alongside MoonBit, Vera-lang, and
  Zerolang. This kernel, the first large program written in Vow, was undertaken
  as a challenge: to have an agent develop a complete Lean kernel entirely in the
  new language, and it doubles as a proving ground for it. The work was 100%
  agentic, carried out with Claude Code.
  :::

def «zignodamus» : VersoDoc Page :=
  verso (Page) "zignodamus"
  :::
  Zig kernel based on [sokonanoda](https://github.com/intgrah/sokonanoda)
  :::

end ArenaSite.Descriptions.Checkers

namespace ArenaSite.Descriptions

def testDescription? : String → Option (Array (Block Page))
  | "Undecidability/alg-conv-trans-acc-left" => some Tests.«Undecidability/alg-conv-trans-acc-left».toPart.content
  | "Undecidability/alg-conv-trans-acc-right" => some Tests.«Undecidability/alg-conv-trans-acc-right».toPart.content
  | "Undecidability/alg-conv-trans-acc" => some Tests.«Undecidability/alg-conv-trans-acc».toPart.content
  | "Undecidability/alg-conv-trans-quot-left-def" => some Tests.«Undecidability/alg-conv-trans-quot-left-def».toPart.content
  | "Undecidability/alg-conv-trans-quot-left" => some Tests.«Undecidability/alg-conv-trans-quot-left».toPart.content
  | "Undecidability/alg-conv-trans-quot-right" => some Tests.«Undecidability/alg-conv-trans-quot-right».toPart.content
  | "Undecidability/alg-conv-trans-quot" => some Tests.«Undecidability/alg-conv-trans-quot».toPart.content
  | "Undecidability/subject-reduction-redex" => some Tests.«Undecidability/subject-reduction-redex».toPart.content
  | "Undecidability/subject-reduction-reduct" => some Tests.«Undecidability/subject-reduction-reduct».toPart.content
  | "nested-nonuniform-param" => some Tests.«nested-nonuniform-param».toPart.content
  | "nested-unused-param" => some Tests.«nested-unused-param».toPart.content
  | "bogus1" => some Tests.«bogus1».toPart.content
  | "cedar" => some Tests.«cedar».toPart.content
  | "constlevels" => some Tests.«constlevels».toPart.content
  | "cslib" => some Tests.«cslib».toPart.content
  | "ctor-num-fields" => some Tests.«ctor-num-fields».toPart.content
  | "init" => some Tests.«init».toPart.content
  | "init-prelude" => some Tests.«init-prelude».toPart.content
  | "k-rec-conv" => some Tests.«k-rec-conv».toPart.content
  | "large-elim-param" => some Tests.«large-elim-param».toPart.content
  | "level-imax-leq" => some Tests.«level-imax-leq».toPart.content
  | "level-imax-normalization" => some Tests.«level-imax-normalization».toPart.content
  | "level-index-out-of-order" => some Tests.«level-index-out-of-order».toPart.content
  | "mathlib" => some Tests.«mathlib».toPart.content
  | "nat-rec-k-lie" => some Tests.«nat-rec-k-lie».toPart.content
  | "nat-rec-rules" => some Tests.«nat-rec-rules».toPart.content
  | "perf/app-lam" => some Tests.«perf/app-lam».toPart.content
  | "perf/grind-ring-5" => some Tests.«perf/grind-ring-5».toPart.content
  | "perf/shift-cascade" => some Tests.«perf/shift-cascade».toPart.content
  | "proj-non-structure" => some Tests.«proj-non-structure».toPart.content
  | "proj-of-prop" => some Tests.«proj-of-prop».toPart.content
  | "proof-irrel" => some Tests.«proof-irrel».toPart.content
  | "rec-k-lie" => some Tests.«rec-k-lie».toPart.content
  | "sparse-name-index" => some Tests.«sparse-name-index».toPart.content
  | "std" => some Tests.«std».toPart.content
  | "tutorial" => some Tests.«tutorial».toPart.content
  | _ => none

def checkerDescription? : String → Option (Array (Block Page))
  | "always-accept" => some Checkers.«always-accept».toPart.content
  | "always-decline" => some Checkers.«always-decline».toPart.content
  | "always-reject" => some Checkers.«always-reject».toPart.content
  | "evmlean" => some Checkers.«evmlean».toPart.content
  | "lean4lean" => some Checkers.«lean4lean».toPart.content
  | "mini" => some Checkers.«mini».toPart.content
  | "nanobruijn" => some Checkers.«nanobruijn».toPart.content
  | "nanoda" => some Checkers.«nanoda».toPart.content
  | "nyaya" => some Checkers.«nyaya».toPart.content
  | "official" => some Checkers.«official».toPart.content
  | "official-nightly" => some Checkers.«official-nightly».toPart.content
  | "official-v4.28.0" => some Checkers.«official-v4.28.0».toPart.content
  | "parse-only" => some Checkers.«parse-only».toPart.content
  | "rpylean" => some Checkers.«rpylean».toPart.content
  | "sokonanoda" => some Checkers.«sokonanoda».toPart.content
  | "still-nanoda" => some Checkers.«still-nanoda».toPart.content
  | "vow-lean-kernel" => some Checkers.«vow-lean-kernel».toPart.content
  | "zignodamus" => some Checkers.«zignodamus».toPart.content
  | _ => none

end ArenaSite.Descriptions