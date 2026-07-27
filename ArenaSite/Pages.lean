import ArenaSite.Pages.Front
import ArenaSite.Pages.Details

open Lean
open Lean.Elab Term Command
open Verso Doc Verso.Genre.Blog

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
  descendants : Array String

partial def buildNodes (prefixPath : String) (entries : Array (List String × TestInfo)) :
    Array TestNode :=
  let heads := entries.foldl (init := #[]) fun heads (path, _) =>
    match path with
    | segment :: _ => if heads.contains segment then heads else heads.push segment
    | [] => heads
  heads.map fun segment =>
    let matching := entries.filter fun (path, _) => path.head? == some segment
    let fullName := if prefixPath.isEmpty then segment else s!"{prefixPath}/{segment}"
    let test := matching.findSome? fun (path, test) => if path.length == 1 then some test else none
    let deeper := matching.filterMap fun (path, test) =>
      if path.length > 1 then some (path.tail, test) else none
    let children := buildNodes fullName deeper
    let descendants :=
      (if test.isSome then #[fullName] else #[]) ++ children.flatMap (·.descendants)
    { segment, fullName, test, children, descendants }

def testNodes (payload : Payload) : Array TestNode :=
  buildNodes "" (payload.tests.map fun test => (test.name.splitOn "/", test))

private def checkerRows (payload : Payload) (index : ResultIndex) (checker : String) :
    Array Details.CheckerRow :=
  payload.tests.filterMap fun test => do
    let result ← findResult index checker test.name
    return { test, result, official := findResult index "official" test.name }

private def testRows (payload : Payload) (index : ResultIndex) (test : TestInfo) :
    Array Details.TestRow :=
  payload.checkers.filterMap fun checker => do
    let result ← findResult index checker.name test.name
    return { checker := checker.name, result }

scoped syntax "generate_arena_pages" : command

private def declarePart (name : Name) (value : TSyntax `term) : CommandElabM Unit := do
  elabCommand (← `(def $(mkIdent name) : Part Page := $value))

private partial def declareTestParts (payload : Payload) (index : ResultIndex)
    (node : TestNode) : CommandElabM Unit := do
  let rate := payload.instructionsPerSecond
  let value ←
    match node.test with
    | some test =>
      liftTermElabM `(Details.testPart $(quote test) $(quote (testRows payload index test))
        $(quote (findResult index "official" test.name)) $(quote rate))
    | none =>
      liftTermElabM `(Details.groupPart $(quote node.fullName) $(quote node.descendants))
  declarePart (testPartName node.fullName) value
  for child in node.children do
    declareTestParts payload index child

elab_rules : command
  | `(generate_arena_pages) => do
    let payload ← liftTermElabM loadPayload
    let index := indexOf payload
    let rate := payload.instructionsPerSecond
    for checker in payload.checkers do
      let value ← liftTermElabM `(Details.checkerPart $(quote checker)
        $(quote (checkerRows payload index checker.name)) $(quote rate))
      declarePart (checkerPartName checker.name) value
    for node in testNodes payload do
      declareTestParts payload index node

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
