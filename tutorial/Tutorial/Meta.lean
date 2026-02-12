import Lean
import Tutorial.TestCaseEnv
import Tutorial.AddConstInfo

open Lean Elab Term Command
open Lean.Parser.Command

def addTestCaseDeclCore (descr? : Option String) (decl : Lean.Declaration) (outcome : Outcome) : CoreM Unit := do
  match outcome with
  | .good => addDecl decl
  | .bad =>
    withOptions (fun o => debug.skipKernelTC.set o true) do
      addDecl decl
  registerTestCase {
    decls := decl.getNames.toArray
    outcome := outcome
    description := descr?
  }

def addTestCaseDecl (descr? : Option String) (declName : Name) (levelParams : List Name) (typeExpr : Expr) (valueExpr : Expr) (outcome : Outcome) (declKind : ConstantKind) : CoreM Unit := do
  let decl ← match declKind with
    | .defn => pure <| .defnDecl {
        name := declName
        levelParams := levelParams
        type := typeExpr
        value := valueExpr
        hints := .opaque
        safety := .safe
      }
    | .thm => pure <| .thmDecl {
        name := declName
        levelParams := levelParams
        type := typeExpr
        value := valueExpr
      }
    | _ => throwError "Unsupported declaration kind in test case: {repr declKind}"
  addTestCaseDeclCore descr? decl outcome

open TSyntax.Compat in -- due to plainDocComments vs. docComment
def elabAndAddTestCaseDecl (descr? : Option (TSyntax ``plainDocComment)) (name : TSyntax ``declId) (type : Term) (value : Term) (outcome : Outcome) (declKind : ConstantKind) : CommandElabM Unit := liftTermElabM do
  let descrStr? ← descr?.mapM (getDocStringText ·)
  let descrStr? := descrStr?.map (·.trimAscii.copy)
  let (declName, lparams) ← match name with
    | `(declId| $n:ident) => pure (n.getId, [])
    | `(declId| $n:ident .{ $[$ls:ident],* }) => pure (n.getId, ls.toList.map (·.getId))
    | _ => throwUnsupportedSyntax
  withLevelNames lparams do
    let typeExpr ← elabTermAndSynthesize type none
    let valueExpr ← elabTermAndSynthesize value (some typeExpr)
    Term.synthesizeSyntheticMVarsNoPostponing
    let typeExpr ← instantiateMVars typeExpr
    if typeExpr.hasMVar then
      throwError "Failed to elaborate type, has remaining metavariables:{indentD typeExpr}"
    let valueExpr ← instantiateMVars valueExpr
    if valueExpr.hasMVar then
      throwError "Failed to elaborate value, has remaining metavariables:{indentD valueExpr}"
    addTestCaseDecl descrStr? declName lparams typeExpr valueExpr outcome declKind

elab descr?:(plainDocComment)? "good_def " name:declId ":" type:term ":=" value:term : command => do
  elabAndAddTestCaseDecl descr? name type value Outcome.good ConstantKind.defn

elab descr?:(plainDocComment)? "bad_def " name:declId ":" type:term ":=" value:term : command => do
  elabAndAddTestCaseDecl descr? name type value Outcome.bad ConstantKind.defn

elab descr?:(plainDocComment)? "good_thm " name:declId ":" type:term ":=" value:term : command => do
  elabAndAddTestCaseDecl descr? name type value Outcome.good ConstantKind.thm

elab descr?:(plainDocComment)? "bad_thm " name:declId ":" type:term ":=" value:term : command => do
  elabAndAddTestCaseDecl descr? name type value Outcome.bad ConstantKind.thm

open TSyntax.Compat in -- due to plainDocComments vs. docComment
def elabRawTestDecl (descr? : Option (TSyntax `Lean.Parser.Command.plainDocComment)) (decl : Term) (outcome : Outcome) : CommandElabM Unit := liftTermElabM do
  let descrStr? ← descr?.mapM (getDocStringText ·)
  let descrStr? := descrStr?.map (·.trimAscii.copy)
  let expectedType := Lean.mkConst ``Lean.Declaration
  let declExpr ← elabTerm decl (some expectedType)
  Term.synthesizeSyntheticMVarsNoPostponing
  let declExpr ← instantiateMVars declExpr
  let decl ← Lean.Meta.MetaM.run' <| unsafe Meta.evalExpr (α := Lean.Declaration) expectedType declExpr
  addTestCaseDeclCore descrStr? decl outcome

elab descr?:(plainDocComment)? "good_decl " decl:term : command => do
  elabRawTestDecl descr? decl .good

elab descr?:(plainDocComment)? "bad_decl " decl:term : command => do
  elabRawTestDecl descr? decl .bad

def addTestCaseCIsCore (descr? : Option String) (cis : Array Lean.ConstantInfo) (outcome : Outcome) : CoreM Unit := do
  addConstInfos cis
  registerTestCase {
    decls := cis.map (·.name)
    outcome := outcome
    description := descr?
  }


open TSyntax.Compat in -- due to plainDocComments vs. docComment
def elabRawTestCIs (descr? : Option (TSyntax `Lean.Parser.Command.plainDocComment)) (cis : Term) (outcome : Outcome) : CommandElabM Unit := liftTermElabM do
  let descrStr? ← descr?.mapM (getDocStringText ·)
  let descrStr? := descrStr?.map (·.trimAscii.copy)
  let expectedType := mkApp (Lean.mkConst ``Array [0]) (Lean.mkConst ``Lean.ConstantInfo)
  let cisExpr ← elabTerm cis (some expectedType)
  let cisExpr ← instantiateMVars cisExpr
  let cis ← Lean.Meta.MetaM.run' <| unsafe Meta.evalExpr (α := Array Lean.ConstantInfo) expectedType cisExpr
  addTestCaseCIsCore descrStr? cis outcome

elab descr?:(plainDocComment)? "good_raw_consts " ci:term : command => do
  elabRawTestCIs descr? ci .good

elab descr?:(plainDocComment)? "bad_raw_consts " ci:term : command => do
  elabRawTestCIs descr? ci .bad

open TSyntax.Compat in -- due to plainDocComments vs. docComment
def elabRawTestConsts (descr? : Option (TSyntax `Lean.Parser.Command.plainDocComment)) (cis : Term) (outcome : Outcome) : CommandElabM Unit := liftTermElabM do
  let descrStr? ← descr?.mapM (getDocStringText ·)
  let descrStr? := descrStr?.map (·.trimAscii.copy)
  let expectedType := mkApp (Lean.mkConst ``Array [0]) (Lean.mkConst ``Lean.Name)
  let namesExpr ← elabTerm cis (some expectedType)
  let namesExpr ← instantiateMVars namesExpr
  let names ← Lean.Meta.MetaM.run' <| unsafe Meta.evalExpr (α := Array Lean.Name) expectedType namesExpr
  let cis ← names.mapM Lean.getConstInfo
  addTestCaseCIsCore descrStr? cis outcome

elab descr?:(plainDocComment)? "good_consts " ci:term : command => do
  elabRawTestConsts descr? ci .good

section Unchecked

/-- An elaborator that just inserts the term, without regard for the acutal type needed here -/
syntax (name := unchecked) "unchecked" term : term

section
open Lean Meta Elab Term


@[term_elab «unchecked»]
def elabUnchecked : TermElab := fun stx expectedType? => do
  match stx with
  | `(unchecked $t) =>
    let some expectedType := expectedType? |
      tryPostpone
      throwError "invalid 'unchecked', expected type required"
    let e ←  elabTerm t none
    let mvar ← mkFreshExprMVar expectedType MetavarKind.syntheticOpaque
    mvar.mvarId!.assign e
    return mvar
  | _ => throwUnsupportedSyntax

end

end Unchecked

/-! Some expression builder helpers -/

def arrow  (dom : Expr) (codom : Expr) (n := `x) : Expr :=
  .forallE n dom codom .default

def dummyRecInfo (indName : Lean.Name) : Lean.ConstantInfo :=
  .recInfo {
      name := indName ++ `rec
      levelParams := []
      type := .sort 0
      all := [indName]
      numParams := 0
      numIndices := 0
      numMotives := 0
      numMinors := 0
      rules := []
      k := false
      isUnsafe := false
  }
