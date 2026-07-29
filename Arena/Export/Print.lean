import SubVerso.Highlighting.Code
import SubVerso.Module
import Lean.Widget.InteractiveCode
import Lean.PrettyPrinter
import Arena.Export.Parse

namespace Arena.Export

open Lean
open Lean.Widget (TaggedText CodeWithInfos tagCodeInfos)
open SubVerso.Highlighting

private def constKind : ConstantInfo → String
  | .axiomInfo _ => "axiom"
  | .defnInfo _ => "def"
  | .thmInfo _ => "theorem"
  | .opaqueInfo _ => "opaque"
  | .quotInfo _ => "quot"
  | .inductInfo _ => "inductive"
  | .ctorInfo _ => "constructor"
  | .recInfo _ => "recursor"

private partial def displayLength : Highlighted → Nat
  | .token t => t.content.length
  | .text s | .unparsed s => s.length
  | .seq hs => hs.foldl (fun total h => total + displayLength h) 0
  | .span _ h => displayLength h
  | .tactics _ _ _ h => displayLength h
  | .point _ _ => 0

private partial def indentBy (spaces : String) : Highlighted → Highlighted
  | .text s => .text (s.replace "\n" ("\n" ++ spaces))
  | .unparsed s => .unparsed (s.replace "\n" ("\n" ++ spaces))
  | .seq hs => .seq (hs.map (indentBy spaces))
  | .span info h => .span info (indentBy spaces h)
  | .tactics info a b h => .tactics info a b (indentBy spaces h)
  | h => h

/--
Strips the first `n` characters of leading text, which `layout` prepends so that the
Wadler-Lindig algorithm sees the column the expression will actually start at.
-/
private partial def stripLeadingText {α : Type} (tt : TaggedText α) (n : Nat) : TaggedText α :=
  if n == 0 then tt
  else match tt with
  | .text s => .text (s.drop n).toString
  | .append items =>
    match items[0]? with
    | some first => .append (#[stripLeadingText first n] ++ items.extract 1 items.size)
    | none => .append items
  | .tag a content => .tag a (stripLeadingText content n)

private def highlightingContext : IO SubVerso.Highlighting.Context := do
  return { ids := {}, definitionsPossible := false, includeUnparsed := false,
           suppressNamespaces := [], sigCache := ← IO.mkRef {} }

private def tagged (fmt : Std.Format) (infos : PrettyPrinter.InfoPerPos)
    (width column : Nat) : MetaM Highlighted := do
  let padded :=
    if column > 0 then .text (String.ofList (List.replicate column ' ')) ++ fmt else fmt
  let laid := TaggedText.prettyTagged padded (w := width)
  let ctx : Elab.ContextInfo := {
    env := ← getEnv, mctx := ← getMCtx, options := ← getOptions,
    currNamespace := ← getCurrNamespace, openDecls := ← getOpenDecls,
    fileMap := default, ngen := ← getNGen
  }
  let code ← tagCodeInfos ctx infos (stripLeadingText laid column)
  ReaderT.run (renderTagged none code) (← highlightingContext)

/--
Pretty-prints `e` as it would appear starting at `column`, so the layout engine breaks
lines where they will actually be too long.
-/
def ppExpr (e : Expr) (width : Nat) (column : Nat := 0) : MetaM Highlighted := do
  let e ← instantiateMVars e
  try
    let ⟨fmt, infos⟩ ← PrettyPrinter.ppExprWithInfos e
    tagged (.nest column fmt) infos width column
  catch _ =>
    try
      return .text ((← Meta.ppExpr e).pretty width 0 column)
    catch _ =>
      return .text e.dbgToString

/--
Pretty-prints `" : type"` with the colon as a soft break point, so the type moves to its own
indented line only when the signature does not fit on one.
-/
private def ppColonType (e : Expr) (width colonColumn : Nat) (breakIndent := 4) :
    MetaM Highlighted := do
  let e ← instantiateMVars e
  try
    let ⟨fmt, infos⟩ ← PrettyPrinter.ppExprWithInfos e
    tagged (.group (.text " :" ++ .nest breakIndent (.line ++ fmt))) infos width colonColumn
  catch _ =>
    try
      return .text (" : " ++ (← Meta.ppExpr e).pretty width)
    catch _ =>
      return .text (" : " ++ e.dbgToString)

private def binderDelimiters : BinderInfo → String × String
  | .default => ("(", ")")
  | .implicit => ("{", "}")
  | .strictImplicit => ("⦃", "⦄")
  | .instImplicit => ("[", "]")

private def levelSuffix (levelParams : List Name) : Highlighted :=
  if levelParams.isEmpty then .text ""
  else
    let params := levelParams.map fun p => (Highlighted.token ⟨.levelVar p, toString p⟩)
    let separated := params.foldl (init := #[]) fun acc p =>
      if acc.isEmpty then #[p] else acc ++ #[.text ", ", p]
    .seq (#[.text ".{"] ++ separated ++ #[.text "}"])

private def ppParameters (type : Expr) (count width : Nat) :
    MetaM (Highlighted × Expr) :=
  Meta.forallBoundedTelescope type (some count) fun binders _ => do
    let mut parts := #[]
    for binder in binders do
      let decl ← binder.fvarId!.getDecl
      let (open', close) := binderDelimiters decl.binderInfo
      let name := if decl.userName.isAnonymous then "_" else toString decl.userName
      let type ← ppExpr decl.type width
      let rendered : Highlighted :=
        .seq #[.text open', .text name, .text " : ", type, .text close]
      parts := parts.push (if parts.isEmpty then rendered else .seq #[.text " ", rendered])
    return (.seq parts, ← Meta.instantiateForall type (binders.map (·.fvarId!) |>.map .fvar))

/--
Pretty-prints a whole declaration: its keyword, name, level parameters, signature and, where
it has one, its value.
-/
def ppDeclaration (ci : ConstantInfo) (width : Nat := 80) : MetaM Highlighted := do
  withOptions (fun o => o.insert `pp.all true) do
    let kind := constKind ci
    let levels := levelSuffix ci.levelParams
    let headLength := kind.length + 1 + (toString ci.name).length + displayLength levels
    let (parameters, colonType) ← match ci with
      | .inductInfo iv =>
        if iv.numParams == 0 then
          pure (Highlighted.text "", ← ppColonType ci.type width headLength)
        else
          let (parameters, body) ← ppParameters ci.type iv.numParams width
          let column := headLength + 1 + displayLength parameters
          pure (.seq #[.text " ", parameters], ← ppColonType body width column)
      | _ => pure (Highlighted.text "", ← ppColonType ci.type width headLength)
    let value ← match ci with
      | .defnInfo v | .thmInfo v | .opaqueInfo v =>
        pure (some (← ppExpr v.value (width - 2)))
      | _ => pure none
    let signature : Highlighted := .seq #[
      .token ⟨.keyword none none none, kind⟩,
      .text " ",
      .token ⟨.const ci.name "" none true none, toString ci.name⟩,
      levels, parameters, colonType
    ]
    match value with
    | some value => return .seq #[signature, .text " :=\n  ", indentBy "  " value]
    | none => return signature

end Arena.Export
