import ArenaSite.Data
import ArenaSite.Copy

open Lean
open Lean.Elab Term
open Verso.Output Html

namespace ArenaSite

open ArenaSite.Copy

def payloadField (project : Payload → α) [Quote α] (type : Name) : TermElabM Expr := do
  let stx : TSyntax `term := quote (project (← loadPayload))
  elabTerm stx (some (mkConst type))

scoped syntax "build_info%" : term

elab_rules : term
  | `(build_info%) => payloadField (·.build) ``BuildInfo

private def separated (label : String) (url : Option String) : Html :=
  let body := match url with
    | some url => {{ <a href={{url}}>{{Html.text true label}}</a> }}
    | none => Html.text true label
  Html.seq #[Html.text true footerSeparator, body]

def buildStamp (info : BuildInfo) : Html :=
  let revision := match info.shortRevision with
    | some revision => separated (revisionLabel revision) info.commitUrl
    | none => Html.empty
  let action := match info.actionRunId with
    | some runId => separated (workflowRunLabel runId) info.actionUrl
    | none => Html.empty
  {{
    <p class="build-stamp">
      {{Html.text true (generatedPrefix ++ info.timestamp)}}{{revision}}{{action}}
    </p>
  }}

end ArenaSite
