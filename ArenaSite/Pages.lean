import ArenaSite.Pages.Front
import ArenaSite.Pages.Details

open Lean
open Lean.Elab Term Command
open Verso Doc Verso.Genre.Blog
open SubVerso.Highlighting (Highlighted)

set_option maxRecDepth 100000
set_option maxHeartbeats 4000000

namespace ArenaSite.Pages

open ArenaSite.Render
open ArenaSite.Copy
open ArenaSite.Pages.Front

def frontProse : Front.Prose := {
  siteIntro := Copy.siteIntro.toPart.content
  scoring := Copy.detailsScoring.toPart.content
  declining := Copy.detailsDeclining.toPart.content
  interface := Copy.detailsInterface.toPart.content
  tutorial := Copy.detailsTutorial.toPart.content
  extending := Copy.detailsExtending.toPart.content
}

def FrontPage : VersoDoc Page :=
  .mk (fun _ => pagePart siteTitle (Front.blocks arena_payload% frontProse)) "{}"

def CheckerIndex : VersoDoc Page :=
  .mk (fun _ => pagePart checkersHeading #[breadcrumb checkersHeading]) "{}"

def TestIndex : VersoDoc Page :=
  .mk (fun _ => pagePart testsHeading #[breadcrumb testsHeading]) "{}"

private def slug (name : String) : String := name.replace "/" "_"

private def checkerPartName (name : String) : Name :=
  Name.mkSimple s!"checkerPart_{slug name}"

private def testPartName (name : String) : Name :=
  Name.mkSimple s!"testPart_{slug name}"

private def qualified (name : Name) : Ident :=
  mkIdent ((`ArenaSite.Pages) ++ name)

private def checkerPageName (name : String) : Name :=
  (`ArenaSite.Pages.Checker).str name

private def testPageName (name : String) : Name :=
  (`ArenaSite.Pages.Test).str name

structure TestNode where
  segment : String
  fullName : String
  test : Option TestInfo
  children : Array TestNode

partial def TestNode.descendants (node : TestNode) : Array String :=
  (if node.test.isSome then #[node.fullName] else #[])
    ++ node.children.flatMap (·.descendants)

partial def buildNodes (prefixPath : String) (entries : Array (List String × TestInfo)) :
    Array TestNode :=
  let (order, groups) := entries.foldl (init := (#[], ({} : Std.HashMap String _)))
    fun (order, groups) (path, test) =>
      match path with
      | [] => (order, groups)
      | segment :: rest =>
        let previous := (Std.HashMap.get? groups segment).getD #[]
        let order := if previous.isEmpty then order.push segment else order
        (order, Std.HashMap.insert groups segment (previous.push (rest, test)))
  order.map fun segment =>
    let members := (Std.HashMap.get? groups segment).getD #[]
    let fullName := if prefixPath.isEmpty then segment else s!"{prefixPath}/{segment}"
    let test := members.findSome? fun (rest, test) => if rest.isEmpty then some test else none
    { segment, fullName, test, children := buildNodes fullName (members.filter (!·.1.isEmpty)) }

def testNodes (payload : Payload) : Array TestNode :=
  buildNodes "" (payload.tests.map fun test => (test.name.splitOn "/", test))

structure PageSources where
  payload : Payload
  index : ResultIndex
  baselines : Std.HashMap String ResultInfo
  nodes : Array TestNode

def pageSources : TermElabM PageSources := do
  let payload ← loadPayload
  let index := indexOf payload
  let baselines := payload.tests.foldl (init := {}) fun map test =>
    match findResult index payload.baselineChecker test.name with
    | some baseline => map.insert test.name baseline
    | none => map
  return { payload, index, baselines, nodes := testNodes payload }

def PageSources.baseline (sources : PageSources) (test : String) : Option ResultInfo :=
  Std.HashMap.get? sources.baselines test

private def checkerRows (sources : PageSources) (checker : String) :
    Array Details.CheckerRow :=
  sources.payload.tests.filterMap fun test => do
    let result ← findResult sources.index checker test.name
    return { test, result, baseline := sources.baseline test.name }

private def testRows (sources : PageSources) (test : TestInfo) : Array Details.TestRow :=
  sources.payload.checkers.filterMap fun checker => do
    let result ← findResult sources.index checker.name test.name
    return { checker := checker.name, result := result.withoutProcessOutput }

scoped syntax "generate_arena_pages" : command

private def declarePart (name : Name) (value : TSyntax `term) : CommandElabM Unit := do
  elabCommand (← `(def $(mkIdent name) : Part Page := $value))

private def sourceName (module : String) : Name :=
  Name.mkSimple s!"source_{module.replace "." "_"}"

private def exportName (test : String) : Name :=
  Name.mkSimple s!"export_{slug test}"

private def sourceModules (sources : PageSources) : Array String :=
  sources.payload.tests.foldl (init := #[]) fun found test =>
    match test.sourceModule with
    | some module => if found.contains module then found else found.push module
    | none => found

private def declareSources (sources : PageSources) : CommandElabM Unit := do
  for module in sourceModules sources do
    let mod ← liftTermElabM (loadModuleSource module)
    let code := Highlighted.seq (mod.items.map (·.code))
    elabCommand (← `(def $(mkIdent (sourceName module)) : Highlighted := $(quote code)))

private def declareExport (test : String) : CommandElabM Bool := do
  let some mod ← liftTermElabM (loadTestExport? test) | return false
  let separated := mod.items.foldl (init := #[]) fun acc item =>
    if acc.isEmpty then #[item.code] else acc ++ #[.text "\n\n", item.code]
  elabCommand (← `(def $(mkIdent (exportName test)) : Highlighted :=
    $(quote (Highlighted.seq separated))))
  return true

private partial def declareTestParts (sources : PageSources) (node : TestNode) :
    CommandElabM Unit := do
  let rate := sources.payload.instructionsPerSecond
  let value ←
    match node.test with
    | some test =>
      let source ← match test.sourceModule with
        | some module => `(some $(qualified (sourceName module)))
        | none => `(none)
      let exported ←
        if ← declareExport test.name then `(some $(qualified (exportName test.name)))
        else `(none)
      liftTermElabM `(Details.testPart $(quote test) $(quote (testRows sources test))
        $(quote (sources.baseline test.name)) $(quote rate) $source $exported)
    | none =>
      liftTermElabM `(Details.groupPart $(quote node.fullName) $(quote node.descendants))
  declarePart (testPartName node.fullName) value
  for child in node.children do
    declareTestParts sources child

elab_rules : command
  | `(generate_arena_pages) => do
    let sources ← liftTermElabM pageSources
    declareSources sources
    let rate := sources.payload.instructionsPerSecond
    for checker in sources.payload.checkers do
      let value ← liftTermElabM `(Details.checkerPart $(quote checker)
        $(quote (checkerRows sources checker.name)) $(quote rate))
      declarePart (checkerPartName checker.name) value
    for node in sources.nodes do
      declareTestParts sources node

generate_arena_pages

scoped syntax "checker_pages%" : term
scoped syntax "test_pages%" : term

private def elabDirArray (terms : Array (TSyntax `term)) : TermElabM Expr := do
  let array ← `(#[$[$terms],*])
  let expected ← elabTerm (← `(Array Dir)) none
  elabTerm array (some expected)

elab_rules : term
  | `(checker_pages%) => do
    let payload ← loadPayload
    let dirs ← payload.checkers.mapM fun checker =>
      `(Dir.page $(quote checker.name) $(quote (checkerPageName checker.name))
        $(qualified (checkerPartName checker.name)) #[])
    elabDirArray dirs

private partial def testNodeDir (node : TestNode) : TermElabM (TSyntax `term) := do
  let children ← node.children.mapM testNodeDir
  `(Dir.page $(quote node.segment) $(quote (testPageName node.fullName))
      $(qualified (testPartName node.fullName)) #[$[$children],*])

elab_rules : term
  | `(test_pages%) => do
    let dirs ← (testNodes (← loadPayload)).mapM testNodeDir
    elabDirArray dirs

end ArenaSite.Pages
