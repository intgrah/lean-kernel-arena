-- Tutorial declarations for Lean type theory features
-- Each declaration exercises a specific feature of the type system

import Tutorial.Meta
set_option linter.unusedVariables false

/-- Basic definition -/
good_def basicDef : Type := Prop

/-- Mismatched types -/
bad_def badDef : Prop := unchecked Type

/-- Arrow type (function type) -/
good_def arrowType : Type := Prop → Prop

/-- Dependent type (forall) -/
good_def dependentType : Prop := ∀ (p: Prop), p

/-- Lambda expression -/
good_def simpleLambda : Type → Type → Type := fun x y => x

/-- Lambda reduction -/
good_def betaReduction : simpleLambda Prop (Prop → Prop) := ∀ p : Prop, p

/-- Lambda reduction under binder -/
good_def betaReduction2 : ∀ (p : Prop), simpleLambda Prop (Prop → Prop) := fun p => p

/-- The type of a declaration has to be a type, not some other expression -/
bad_def nonTypeType : simpleLambda := unchecked Prop

/-- Some level computation -/
good_decl (.defnDecl {
    name := `levelComp1
    levelParams := []
    type := .sort 1
    value := .sort (.imax 1 0)
    hints := .opaque
    safety := .safe
  })

/-- Some level computation -/
good_decl (.defnDecl {
    name := `levelComp2
    levelParams := []
    type := .sort 2
    value := .sort (.imax 0 1)
    hints := .opaque
    safety := .safe
  })

/-- Some level computation -/
good_decl (.defnDecl {
    name := `levelComp3
    levelParams := []
    type := .sort 3
    value := .sort (.imax 2 1)
    hints := .opaque
    safety := .safe
  })

def levelParamF.{u} : Sort u → Sort u → Sort u := fun α β => α

/-- Level parameters -/
good_def levelParams : levelParamF Prop (Prop → Prop) := ∀ p : Prop, p

/-- Duplicate universe paramers -/
bad_decl .defnDecl {
  name := `tut06_bad01
  levelParams := [`u, `u]
  type := .sort 1
  value := .sort 0
  hints := .opaque
  safety := .safe
}

/-- Some level computation -/
good_def levelComp4.{u} : Type 0 := Sort (imax u 0)

/-- Some level computation -/
good_def levelComp5.{u} : Type u := Sort (imax u u)

/-- Type inference for forall using imax -/
good_def imax1 : (p : Prop) → Prop := fun p => Type → p

/-- Type inference for forall using imax -/
good_def imax2 : (α : Type) → Type 1 := fun α => Type → α

/-- Type inference of local variables -/
good_def inferVar : ∀ (f : Prop) (g : f), f := fun f g => g

/-- Definitional equality between lambdas -/
good_def defEqLambda : ∀ (f : (Prop → Prop) → Prop) (g : (a : Prop → Prop) → f a), f (fun p => p → p) :=
  fun f g => g (fun p => p → p)

/-! Let's build peano arithmetic -/

def PN := ∀ α, (α → α) → (α → α)
def PN.zero : PN := fun α s z => z
def PN.succ : PN → PN := fun n α s z => s (n α s z)

def PN.lit0 := PN.zero
def PN.lit1 := PN.succ PN.lit0
def PN.lit2 := PN.succ PN.lit1
def PN.lit3 := PN.succ PN.lit2
def PN.lit4 := PN.succ PN.lit3

def PN.add : PN → PN → PN := fun n m α s z => n α s (m α s z)
def PN.mul : PN → PN → PN := fun n m α s z => n α (m α s) z


/-- Peano arithmetic: 2 = 2 -/
good_thm peano1.{u} : ∀ (t : PN → Prop) (v : (n : PN) → t n), t PN.lit2.{u} :=
  fun t v => v PN.lit2.{u}

/-- Peano arithmetic: 1 + 1 = 2 -/
good_thm peano2.{u} : ∀ (t : PN → Prop) (v : (n : PN) → t n), t PN.lit2.{u} :=
  fun t v => v (PN.lit1.add PN.lit1)

/-- Peano arithmetic: 2 * 2 = 4 -/
good_thm peano3.{u} : ∀ (t : PN → Prop) (v : (n : PN) → t n), t PN.lit4.{u} :=
  fun t v => v (PN.lit2.mul PN.lit2)


inductive N : Type where | zero : N | succ : N → N

/-- A first simple but recursive inductive data type -/
good_def natDef : Type := N

good_thm natDefRec :
    ∀ (motive : N → Prop) (zero : motive N.zero) (succ: ∀ n, motive n → motive (N.succ n)),
    let r := @N.rec motive zero succ;
    (r .zero = zero) ∧ (∀ n, r (.succ n) = succ n (r n)) := by
  intros
  constructor
  · rfl
  · intro; rfl

/-- An inductive type with a non-sort type -/
bad_consts #[
  .inductInfo {
      name := `inductBadNonSort
      levelParams := []
      type := .const `simpleLambda []
      numParams := 0
      numIndices := 0
      all := [`inductBadNonSort]
      ctors := []
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }]

axiom aType : Type

/-- Another inductive type with a non-sort type -/
bad_consts #[
  .inductInfo {
      name := `inductBadNonSort2
      levelParams := []
      type := .const `aType []
      numParams := 0
      numIndices := 0
      all := [`inductBadNonSort2]
      ctors := []
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }]

/-- An inductive with duplicate level params -/
bad_consts #[
  .inductInfo {
      name := `inductLevelParam
      levelParams := [`u, `u]
      type := .sort 1
      numParams := 0
      numIndices := 0
      all := [`inductLevelParam]
      ctors := []
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }]

/-- An inductive with too few parameters in the type -/

bad_consts #[
  .inductInfo {
      name := `inductTooFewParams
      levelParams := []
      type := .forallE `x (.sort 0) (.sort 0) .default
      numParams := 2
      numIndices := 0
      all := [`inductTooFewParams]
      ctors := []
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }]
