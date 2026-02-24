import Lean.Expr
import TestPrinter.Types

namespace TestPrinter

open Lean

/-- Pretty-print a universe level. -/
partial def ppLevel (l : Level) : String :=
  match l with
  | .zero => "0"
  | .succ .zero => "1"
  | .succ l => s!"({ppLevel l} + 1)"
  | .max l1 l2 => s!"(max {ppLevel l1} {ppLevel l2})"
  | .imax l1 l2 => s!"(imax {ppLevel l1} {ppLevel l2})"
  | .param n => toString n
  | .mvar _ => "?u"

/-- Pretty-print a universe level for Sort. -/
def ppSort (l : Level) : String :=
  match l with
  | .zero => "Prop"
  | .succ .zero => "Type"
  | .succ l => s!"Type {ppLevel l}"
  | _ => s!"Sort {ppLevel l}"

private def binderInfoOpen : BinderInfo → String
  | .default => "("
  | .implicit => "{"
  | .strictImplicit => "⦃"
  | .instImplicit => "["

private def binderInfoClose : BinderInfo → String
  | .default => ")"
  | .implicit => "}"
  | .strictImplicit => "⦄"
  | .instImplicit => "]"

/-- Check if a Name is an internal hygiene name (contains `_@` or `_hyg` components). -/
private def isHygieneName : Name → Bool
  | .str _ s => s == "_@" || s == "_hyg" || s == "_internal"
  | .num p _ => isHygieneName p
  | .anonymous => false

/-- Clean up internal hygiene names like `a._@._internal._hyg.0` → `_`. -/
private def cleanName (n : Name) : String :=
  if n.isAnonymous then "_"
  else if isHygieneName n then "_"
  else toString n

private def indentStr (n : Nat) : String :=
  String.ofList (List.replicate n ' ')

/-- Pretty-print an expression. Uses a name context for de Bruijn indices.
    `col` is the current column, `width` is the target max width. -/
partial def ppExpr (e : Expr) (ctx : List Name := []) (col : Nat := 0) (width : Nat := 72) : String :=
  match e with
  | .bvar n =>
    match ctx[n]? with
    | some nm => cleanName nm
    | none => s!"#{n}"
  | .sort l => ppSort l
  | .const name [] => toString name
  | .const name us =>
    let usStr := ", ".intercalate (us.map ppLevel)
    toString name ++ ".{" ++ usStr ++ "}"
  | .app f a => ppApp f a ctx col width
  | .lam _ _ _ _ => ppLams e ctx col width
  | .forallE _ _ _ _ => ppForalls e ctx col width
  | .letE name ty val body _ =>
    let nameStr := cleanName name
    let tyStr := ppExpr ty ctx col width
    let header := s!"let {nameStr} : {tyStr} := "
    let valStr := ppExpr val ctx (col + header.length) width
    let flat := s!"{header}{valStr}; {ppExpr body (name :: ctx) col width}"
    if col + flat.length ≤ width then flat
    else
      let ind := col + 2
      s!"{header}{valStr};\n{indentStr ind}{ppExpr body (name :: ctx) ind width}"
  | .lit (.natVal n) => toString n
  | .lit (.strVal s) => s!"\"{s}\""
  | .mdata _ e => ppExpr e ctx col width
  | .proj typeName idx struct =>
    s!"{ppExpr struct ctx col width}.{typeName}.{idx}"
  | .fvar fvarId => s!"?fvar.{fvarId.name}"
  | .mvar mvarId => s!"?mvar.{mvarId.name}"
where
  /-- Flatten application spine and print. -/
  ppApp (f : Expr) (a : Expr) (ctx : List Name) (col : Nat) (width : Nat) : String :=
    let args := collectArgs f [a]
    let flat := "(" ++ " ".intercalate (args.map (ppExpr · ctx 0 width)) ++ ")"
    if col + flat.length ≤ width then flat
    else
      let ind := col + 2
      let parts := args.map (ppExpr · ctx ind width)
      "(" ++ ("\n" ++ indentStr ind).intercalate parts ++ ")"
  collectArgs (e : Expr) (acc : List Expr) : List Expr :=
    match e with
    | .app f a => collectArgs f (a :: acc)
    | _ => e :: acc
  /-- Collect and print consecutive lambda binders. -/
  ppLams (e : Expr) (ctx : List Name) (col : Nat) (width : Nat) : String :=
    let (binders, body, ctx') := collectLams e ctx col width
    let binderStr := " ".intercalate binders
    let header := s!"fun {binderStr} => "
    let flat := s!"{header}{ppExpr body ctx' 0 width}"
    if col + flat.length ≤ width then flat
    else
      let ind := col + 2
      s!"{header}\n{indentStr ind}{ppExpr body ctx' ind width}"
  collectLams (e : Expr) (ctx : List Name) (col : Nat) (width : Nat) :
      (List String × Expr × List Name) :=
    match e with
    | .lam name ty body bi =>
      let nameStr := cleanName name
      let tyStr := ppExpr ty ctx col width
      let binder := s!"{binderInfoOpen bi}{nameStr} : {tyStr}{binderInfoClose bi}"
      let (rest, finalBody, ctx') := collectLams body (name :: ctx) col width
      (binder :: rest, finalBody, ctx')
    | _ => ([], e, ctx)
  /-- Collect and print consecutive forall binders; use → for non-dependent. -/
  ppForalls (e : Expr) (ctx : List Name) (col : Nat) (width : Nat) : String :=
    match e with
    | .forallE name ty body bi =>
      if body.hasLooseBVars then
        -- Dependent: collect consecutive dependent foralls
        let nameStr := cleanName name
        let tyStr := ppExpr ty ctx col width
        let binder := s!"{binderInfoOpen bi}{nameStr} : {tyStr}{binderInfoClose bi}"
        let bodyStr := ppForalls body (name :: ctx) col width
        if bodyStr.startsWith "∀ " then
          -- Merge with following ∀
          let flat := s!"∀ {binder} " ++ bodyStr.drop 2
          if col + flat.length ≤ width then flat
          else
            let ind := col + 2
            s!"∀ {binder},\n{indentStr ind}{ppForalls body (name :: ctx) ind width}"
        else
          let flat := s!"∀ {binder}, {bodyStr}"
          if col + flat.length ≤ width then flat
          else
            let ind := col + 2
            s!"∀ {binder},\n{indentStr ind}{ppForalls body (name :: ctx) ind width}"
      else
        -- Non-dependent arrow
        let tyStr := ppExpr ty ctx col width
        let bodyStr := ppExpr body (name :: ctx) col width
        let flat := s!"{tyStr} → {bodyStr}"
        if col + flat.length ≤ width then flat
        else
          let ind := col + 2
          s!"{tyStr} →\n{indentStr ind}{ppExpr body (name :: ctx) ind width}"
    | _ => ppExpr e ctx col width

/-- Get the kind label for a ConstantInfo. -/
def constKind (ci : ConstantInfo) : String :=
  match ci with
  | .axiomInfo _ => "axiom"
  | .defnInfo _ => "def"
  | .thmInfo _ => "theorem"
  | .opaqueInfo _ => "opaque"
  | .quotInfo _ => "quot"
  | .inductInfo _ => "inductive"
  | .ctorInfo _ => "constructor"
  | .recInfo _ => "recursor"

/-- Pretty-print a single ConstantInfo. -/
def ppConstantInfo (ci : ConstantInfo) (width : Nat := 72) : PrettyDecl :=
  let nameStr := toString ci.name
  let kind := constKind ci
  -- Column offset: "kind name : " prefix
  let typeCol := kind.length + 1 + nameStr.length + 3
  let typePP := ppExpr ci.type [] typeCol width
  -- Value starts at indent 2 (after ":=\n  ")
  let valuePP := match ci with
    | .defnInfo v => some (ppExpr v.value [] 2 width)
    | .thmInfo v => some (ppExpr v.value [] 2 width)
    | .opaqueInfo v => some (ppExpr v.value [] 2 width)
    | _ => none
  {
    kind
    name := ci.name
    levelParams := ci.levelParams
    typePP
    valuePP
  }

end TestPrinter
