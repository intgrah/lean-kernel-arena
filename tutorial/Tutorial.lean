import Tutorial.Meta
set_option linter.unusedVariables false
/-!
Tutorial declarations for Lean type theory features
Each declaration exercises a specific feature of the type system
-/


/-- Basic definition -/
good_def basicDef : Type := Prop

/-- Mismatched types -/
bad_def badDef : Prop := unchecked Type

/-- Arrow type (function type) -/
good_def arrowType : Type := Prop → Prop

/-- Dependent type (forall) -/
good_def dependentType : Prop := ∀ (p: Prop), p

/-- Lambda expression -/
good_def constType : Type → Type → Type := fun x y => x

/-- Lambda reduction -/
good_def betaReduction : constType Prop (Prop → Prop) := ∀ p : Prop, p

/-- Lambda reduction under binder -/
good_def betaReduction2 : ∀ (p : Prop), constType Prop (Prop → Prop) := fun p => p

/-- The type of a declaration has to be a type, not some other expression -/
bad_def nonTypeType : constType := unchecked Prop

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

/-!
Inductives. We begin with examples of good and bad inductive types and constructors.
-/

/-- A simple empty inductive type -/
good_def empty : Type := Empty

/-- A simple enumeration inductive type -/
good_def boolType : Type := Bool

structure TwoBool where
  b1 : Bool
  b2 : Bool

/-- A simple product type -/
good_def twoBool : Type := TwoBool

/-- A parametrized product type (no level parameters) -/
good_def andType : Prop → Prop → Prop := And

/-- A parametrized product type (with level parameters)-/
good_def prodType : Type → Type → Type := Prod

/-- Level-polymorphic unit type -/
good_def pUnitType : Type := PUnit

/-- Equality, as an important indexed non-recursive data type -/
good_def eqType.{u_1} : {α : Sort u_1} → α → α → Prop := @Eq

inductive N : Type where | zero : N | succ : N → N

/-- A recursive inductive data type -/
good_def natDef : Type := N

/-! Now a bunch of illformed inductive types. -/

/-- An inductive type with a non-sort type -/
bad_consts
  let n := `inductBadNonSort
  #[ .inductInfo {
      name := n
      levelParams := []
      type := .const `constType []
      numParams := 0
      numIndices := 0
      all := [n]
      ctors := []
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }]

axiom aType : Type
axiom aProp : Prop

/-- Another inductive type with a non-sort type -/
bad_consts
  let n := `inductBadNonSort2
  #[ .inductInfo {
      name := n
      levelParams := []
      type := .const `aType []
      numParams := 0
      numIndices := 0
      all := [n]
      ctors := []
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }]

/-- An inductive with duplicate level params -/
bad_consts
  let n := `inductLevelParam
  #[ .inductInfo {
      name := n
      levelParams := [`u, `u]
      type := .sort 1
      numParams := 0
      numIndices := 0
      all := [n]
      ctors := []
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }]

/-- An inductive with too few parameters in the type -/

bad_consts
  let n := `inductTooFewParams
  #[ .inductInfo {
      name := n
      levelParams := []
      type := .forallE `x (.sort 0) (.sort 0) .default
      numParams := 2
      numIndices := 0
      all := [n]
      ctors := []
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }]


/-- An inductive with a constructor with wrong parameters -/
bad_consts
  let n := `inductWrongCtorParams
  #[ .ctorInfo {
      name := n ++ `mk
      levelParams := []
      type := arrow (.sort 1) ((Lean.mkConst n).app (.const `aProp []))
      numParams := 1
      induct := n
      cidx := 0
      numFields := 0
      isUnsafe := false
  },
  -- The exporter insists on some recursor to exist
  dummyRecInfo n,
  .inductInfo {
      name := n
      levelParams := []
      type := arrow (.sort 0) (.sort 1)
      numParams := 1
      numIndices := 0
      all := [n]
      ctors := [n ++ `mk]
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }
  ]

/-- An inductive with a constructor with wrong parameters in result (they are swapped) -/
bad_consts
  let n := `inductWrongCtorResParams
  #[ .ctorInfo {
      name := n ++ `mk
      levelParams := []
      type := arrow (n := `x) (.sort 0) <| arrow (n := `y) (.sort 0) <| Lean.mkApp2 (Lean.mkConst n) (.bvar 0) (.bvar 1)
      numParams := 2
      induct := n
      cidx := 0
      numFields := 0
      isUnsafe := false
  },
  -- The exporter insists on some recursor to exist
  dummyRecInfo n,
  .inductInfo {
      name := n
      levelParams := []
      type := arrow (n := `x) (.sort 0) <| arrow (n := `y) (.sort 0) <| .sort 1
      numParams := 2
      numIndices := 0
      all := [n]
      ctors := [n ++ `mk]
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }
  ]

/-- An inductive with a constructor with wrong level parameters in result (they are swapped) -/
bad_consts
  let n := `inductWrongCtorResLevel
  #[ .ctorInfo {
      name := n ++ `mk
      levelParams := [`u1, `u2]
      type := arrow (n := `x) (.sort 0) <| arrow (n := `y) (.sort 0) <|
        Lean.mkApp2 (Lean.mkConst n [.param `u2,.param `u1]) (.bvar 1) (.bvar 0)
      numParams := 2
      induct := n
      cidx := 0
      numFields := 0
      isUnsafe := false
  },
  -- The exporter insists on some recursor to exist
  dummyRecInfo n,
  .inductInfo {
      name := n
      levelParams := [`u1,`u2]
      type := arrow (n := `x) (.sort 0) <| arrow (n := `y) (.sort 0) <| .sort 1
      numParams := 2
      numIndices := 0
      all := [n]
      ctors := [n ++ `mk]
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }
  ]

/-- A constructor with an unexpected occurrence of the type in index position of a return type. -/
bad_consts
  let n := `inductInIndex
  #[ .ctorInfo {
      name := n ++ `mk
      levelParams := []
      type := Lean.mkApp (Lean.mkConst n) (Lean.mkApp (Lean.mkConst n) (Lean.mkConst ``aProp))
      numParams := 0
      induct := n
      cidx := 0
      numFields := 0
      isUnsafe := false
  },
  -- The exporter insists on some recursor to exist
  dummyRecInfo n,
  .inductInfo {
      name := n
      levelParams := []
      type := arrow (.sort 0) (.sort 0)
      numParams := 0
      numIndices := 1
      all := [n]
      ctors := [n ++ `mk]
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }
  ]

/-- The classic example of an inductive with negative recursive occurrence -/
bad_consts
  let n := `indNeg
  #[ .ctorInfo {
      name := n ++ `mk
      levelParams := []
      type := arrow (arrow (.const n []) (.const n [])) (.const n [])
      numParams := 0
      induct := n
      cidx := 0
      numFields := 1
      isUnsafe := false
  },
  -- The exporter insists on some recursor to exist
  dummyRecInfo n,
  .inductInfo {
      name := n
      levelParams := []
      type := .sort 1
      numParams := 0
      numIndices := 0
      all := [n]
      ctors := [n ++ `mk]
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }
  ]

/--
When checking inductives, we expect the kernel to reduce the types of constructor arguments.
-/
-- This test needs to be written using `good_decl` because the surface syntax does not allow
-- us to control the type of the constructor parameters.
good_decl
  let n := `reduceCtorParam
  .inductDecl (lparams := []) (nparams := 1) (isUnsafe := false) [{
    name := n
    type := arrow (.sort 1) (.sort 1)
    ctors := [{
        name := n ++ `mk
        type :=
          arrow (n := `α) (Lean.mkApp2 (Lean.mkConst ``id [3]) (.sort 2) (.sort 1)) <|
          arrow (Lean.mkApp2 (Lean.mkConst ``constType) ((Lean.mkConst n []).app (.bvar 0)) ((Lean.mkConst n []).app (.bvar 0))) <|
          Lean.mkApp (Lean.mkConst n) (.bvar 1)
    }]
  }]

/--
When checking inductives, we expect the kernel to **not** reduce the type of the constructor itself;
that should be all manifest `forall`s
-/
bad_consts
  let n := `reduceCtorType
  #[ .inductInfo {
      name := n
      levelParams := []
      type := .sort 1
      numParams := 0
      numIndices := 0
      all := [n]
      ctors := [n ++ `mk]
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  },
  dummyRecInfo n,
  .ctorInfo {
      name := n ++ `mk
      levelParams := []
      type := Lean.mkApp2 (.const ``id [2]) (.sort 1) (Lean.mkConst n)
      numParams := 0
      induct := n
      cidx := 0
      numFields := 0
      isUnsafe := false
  }
  ]

/--
When checking inductives, we expect the kernel to **not** reduce the type of the constructor parameters
further than head normal form. Recursive occurrences nested inside the head normal form are considered
negative occurrences, even if they could be reduced to disappear.
-/
bad_consts
  let n := `indNegReducible
  #[ .ctorInfo {
      name := n ++ `mk
      levelParams := []
      type := arrow (arrow (Lean.mkApp2 (.const ``constType []) (.const ``aType []) (.const n [])) (.const n [])) (.const n [])
      numParams := 0
      induct := n
      cidx := 0
      numFields := 1
      isUnsafe := false
  },
  -- The exporter insists on some recursor to exist
  dummyRecInfo n,
  .inductInfo {
      name := n
      levelParams := []
      type := .sort 1
      numParams := 0
      numIndices := 0
      all := [n]
      ctors := [n ++ `mk]
      numNested := 0
      isRec := false
      isUnsafe := false
      isReflexive := false
  }
  ]


/-! Now statically checking the recursors -/

/-- Asserting the type of the generated recursor -/
good_def emptyRec.{u} : ∀ (motive : Empty → Sort u) (x : Empty), motive x := @Empty.rec

/-- Asserting the type of the generated recursor -/
good_def boolRec.{u} : ∀ {motive : Bool → Sort u} (false : motive false) (true : motive true) (t : Bool), motive t := Bool.rec

/-- Asserting the type of the generated recursor -/
good_def twoBoolRec.{u} : ∀ {motive : TwoBool → Sort u} (mk : ∀ b1 b2, motive ⟨b1, b2⟩) (x : TwoBool), motive x := TwoBool.rec

/-- Asserting the type of the generated recursor -/
good_def andRec.{u} : ∀ (p q : Prop) {motive : And p q → Sort u} (mk : ∀ p q, motive (And.intro p q)) (x : And p q), motive x := @And.rec

/-- Asserting the type of the generated recursor -/
good_def prodRec.{u,v,w} : ∀ (α : Type u) (β : Type v) {motive : Prod α β → Sort u} (mk : ∀ p q, motive (.mk p q)) (x : Prod α β), motive x := @Prod.rec

/-- Asserting the type of the generated recursor -/
good_def punitRec.{u,w} : ∀ {motive : PUnit.{u} → Sort w} (mk : motive ⟨⟩) (x : PUnit), motive x := @PUnit.rec

/-- Asserting the type of the generated recursor -/
good_def eqRec.{u, u_1} : ∀ {α : Sort u_1} {a : α} {motive : (a' : α) → a = a' → Sort u} (refl : motive a (.refl a)) {a' : α}
  (t : a = a'), motive a' t := @Eq.rec

inductive BoolProp : Prop where
  | a : BoolProp
  | b : BoolProp

/-- Inductive predicates eliminiate into Prop if they have more than one construtor. -/
good_def boolPropRec : ∀ {motive : BoolProp → Prop} (a : motive BoolProp.a) (b : motive BoolProp.b) (x : BoolProp), motive x := @BoolProp.rec

/-- Inductive predicates eliminiate into Prop if they have one constructors and it carries data. -/
good_def existsRec.{u} : ∀ {α : Sort u} {p : α → Prop} {motive : Exists p → Prop} (intro : ∀ (w : α) (h : p w), motive ⟨w,h⟩)
  (t : Exists p), motive t := @Exists.rec


inductive SortElimProp (b : Bool) : Bool → Bool → Prop
  | mk (b1 b2 : Bool) : SortElimProp b b2 b1

/--
Inductive predicates eliminiate into Sort if they have one constructors and it carries data, but the data is
known from the type, e.g. a parameter or an index
-/
good_def sortElimPropRec.{u} : ∀ {b : Bool} {motive : ∀ b1 b2, SortElimProp b b1 b2 → Sort u}
  (mk : ∀ b1 b2, motive b2 b1 (.mk b1 b2)) (b1 b2 : Bool) (x : SortElimProp b b1 b2), motive b1 b2 x := @SortElimProp.rec

inductive SortElimProp2 (b : Bool) : Bool → Bool → Prop
  | mk (b1 b2 : Bool) : SortElimProp2 b b2 (id b1)

/--
Inductive predicates eliminiate into Sort if they have one constructors and it carries data, but the data is
known from the type, e.g. a parameter or an index. However, it must occur directliy in the result type,
with no intervening reduction.
-/
good_def sortElimProp2Rec : ∀ {b : Bool} {motive : ∀ b1 b2, SortElimProp2 b b1 b2 → Prop}
  (mk : ∀ b1 b2, motive b2 b1 (.mk b1 b2)) (b1 b2 : Bool) (x : SortElimProp2 b b1 b2), motive b1 b2 x := @SortElimProp2.rec

/-! Now actually reducing the recursor -/

good_thm natDefRec :
    ∀ (motive : N → Prop) (zero : motive N.zero) (succ: ∀ n, motive n → motive (N.succ n)),
    let r := @N.rec motive zero succ;
    (r .zero = zero) ∧ (∀ n, r (.succ n) = succ n (r n)) := by
  intros
  constructor
  · rfl
  · intro; rfl

-- TODO:
-- * level constraints on constructors
-- * reflexive inductives
