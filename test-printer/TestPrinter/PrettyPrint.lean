import SubVerso.Highlighting.Code
import Lean.Widget.InteractiveCode
import Lean.PrettyPrinter
import TestPrinter.Types

namespace TestPrinter

open Lean
open Lean.Widget (TaggedText CodeWithInfos SubexprInfo tagCodeInfos)
open SubVerso.Highlighting

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

/-- Compute display length of highlighted content (total character count). -/
private partial def hlLength : Highlighted → Nat
  | .token t => t.content.length
  | .text s => s.length
  | .seq hs => hs.foldl (fun acc h => acc + hlLength h) 0
  | .span _ h => hlLength h
  | .unparsed s => s.length
  | .tactics _ _ _ h => hlLength h
  | .point _ _ => 0

/-- Strip the first `n` characters from the text content at the start of a
    TaggedText. Used to remove a dummy prefix that was prepended for
    column-offset layout. Assumes the first `n` characters are untagged text. -/
private partial def stripLeadingText {α : Type} (tt : TaggedText α) (n : Nat) : TaggedText α :=
  if n == 0 then tt
  else match tt with
  | .text s => .text (s.drop n).toString
  | .append items =>
    if items.isEmpty then .append items
    else .append (#[stripLeadingText items[0]! n] ++ items.extract 1 items.size)
  | .tag a content => .tag a (stripLeadingText content n)

/-- Like `ppExprTagged` but respects a custom layout width and starting column.
    The `column` parameter tells the layout engine where on the line the
    expression will appear, so it makes correct line-breaking decisions.
    Falls back to plain text if the delaborator fails. -/
private def ppExprTaggedW (e : Expr) (width : Nat) (column : Nat := 0)
    : MetaM CodeWithInfos := do
  let e ← instantiateMVars e
  try
    let ⟨fmt, infos⟩ ← PrettyPrinter.ppExprWithInfos e
    -- Prepend a dummy prefix so the layout engine sees the correct column,
    -- and nest by `column` so hard line breaks indent to the right level.
    let fmt' := if column > 0
      then .text (String.ofList (List.replicate column ' ')) ++ .nest column fmt
      else fmt
    let tt := TaggedText.prettyTagged fmt' (w := width)
    -- Strip the dummy prefix from the laid-out result
    let tt' := if column > 0 then stripLeadingText tt column else tt
    let ctx : Elab.ContextInfo := {
      env           := (← getEnv)
      mctx          := (← getMCtx)
      options       := (← getOptions)
      currNamespace := (← getCurrNamespace)
      openDecls     := (← getOpenDecls)
      fileMap       := default
      ngen          := (← getNGen)
    }
    tagCodeInfos ctx infos tt'
  catch _ =>
    try
      let fmt ← Meta.ppExpr e
      pure <| .text (fmt.pretty width 0 column)
    catch _ =>
      pure <| .text e.dbgToString

/-- Pretty-print an expression using Lean's built-in pretty-printer (with custom
    width and column offset) and SubVerso's semantic token rendering.
    Returns `Highlighted` with full hover/binding info. -/
def ppExprHighlighted (e : Expr) (width : Nat) (column : Nat := 0)
    : MetaM Highlighted := do
  let cwi ← ppExprTaggedW e width column
  let ctx : SubVerso.Highlighting.Context :=
    { ids := {}, definitionsPossible := false,
      includeUnparsed := false, suppressNamespaces := [] }
  renderTagged none cwi |>.run ctx

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

/-- Pretty-print a single ConstantInfo, producing highlighted output.
    Runs in MetaM to use Lean's Wadler-Lindig layout algorithm.
    The type is laid out knowing its column position after `kind name : `,
    so the pretty-printer makes correct line-breaking decisions.
    For inductives, parameters are split out before the colon. -/
def ppConstantInfo (ci : ConstantInfo) (width : Nat := 80) : MetaM PrettyDecl := do
  withOptions (fun o => o.insert `pp.all true) do
    let kind := constKind ci
    let nameStr := toString ci.name
    let lvlStr := if ci.levelParams.isEmpty then ""
      else ".{" ++ ", ".intercalate (ci.levelParams.map toString) ++ "}"
    -- For inductives, split type into parameters (before :) and indices (after :)
    let (paramsPP, typePP) ← match ci with
      | .inductInfo iv =>
        if iv.numParams == 0 then
          let prefixLen := kind.length + 1 + nameStr.length + lvlStr.length + 3
          let typePP ← ppExprHighlighted ci.type width prefixLen
          pure (none, typePP)
        else
          Meta.forallBoundedTelescope ci.type (some iv.numParams) fun paramFVars bodyExpr => do
            -- Build highlighted binders for each parameter
            let mut parts : Array Highlighted := #[]
            for fvar in paramFVars do
              let decl ← fvar.fvarId!.getDecl
              let bi := decl.binderInfo
              let pName := if decl.userName.isAnonymous then "_" else toString decl.userName
              let tyPP ← ppExprHighlighted decl.type width 0
              let binder : Highlighted := .seq #[
                .text (binderInfoOpen bi), .text pName, .text " : ", tyPP,
                .text (binderInfoClose bi)]
              parts := parts.push binder
            -- Join params with spaces
            let paramsHL : Highlighted := .seq (parts.foldl (init := #[]) fun acc p =>
              if acc.isEmpty then #[p] else acc ++ #[.text " ", p])
            -- Compute prefix length for the index type column offset
            let paramsTextLen := hlLength paramsHL
            let prefixLen := kind.length + 1 + nameStr.length + lvlStr.length +
              1 + paramsTextLen + 3  -- " " + params + " : "
            let bodyPP ← ppExprHighlighted bodyExpr width prefixLen
            pure (some paramsHL, bodyPP)
      | _ =>
        let prefixLen := kind.length + 1 + nameStr.length + lvlStr.length + 3
        let typePP ← ppExprHighlighted ci.type width prefixLen
        pure (none, typePP)
    -- Value appears after ":=\n  " (indented 2), so use width-2 at column 0.
    -- The renderer adds the 2-space indent prefix for the first line;
    -- internal line breaks are relative to column 0 which aligns with that.
    let valuePP ← match ci with
      | .defnInfo v => some <$> ppExprHighlighted v.value (width - 2)
      | .thmInfo v => some <$> ppExprHighlighted v.value (width - 2)
      | .opaqueInfo v => some <$> ppExprHighlighted v.value (width - 2)
      | _ => pure none
    return { kind, name := ci.name,
             levelParams := ci.levelParams, paramsPP, typePP, valuePP }

end TestPrinter
