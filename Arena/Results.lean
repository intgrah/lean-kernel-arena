import Arena.Config
import Arena.Proc

open Lean (Json ToJson FromJson toJson fromJson?)

namespace Arena

structure ExportMeta where
  exporterVersion : Option String := none
  leanVersion : Option String := none
deriving Inhabited

structure TestStats extends ExportMeta where
  name : String
  size : Nat
  lines : Nat
  expectation : Option Expectation
  comparePerf : Bool := false
  generatedDescription : Option String := none
  declarationUrl : Option String := none
  sourceUrl : Option String := none
  sourceModule : Option String := none
  hash : String := ""
  recipeHash : String := ""
deriving Inhabited, ToJson, FromJson

structure LocatedTest extends TestStats where
  ndjsonPath : System.FilePath

structure ResultEntry extends Metrics where
  test : String
  testHash : String
  attempt : Attempt
  runAt : String
deriving Inhabited, ToJson, FromJson

def ResultEntry.status (entry : ResultEntry) : Status :=
  entry.attempt.status

def ResultEntry.correctness (entry : ResultEntry) (expectation : Option Expectation) :
    Correctness :=
  Correctness.of entry.status expectation

structure ResultLog where
  checker : String
  revision : String
  recipeHash : String
  runner : String
  runAt : String
  sourceUrl : String := ""
  entries : Array ResultEntry
deriving Inhabited, ToJson, FromJson

def loadRevisionLogs (checker revision : String) : IO (Array ResultLog) := do
  let dir := resultsDir / checker / revision
  unless ← dir.pathExists do return #[]
  let mut logs := #[]
  for name in ← findNamesIn dir ".json" do
    match fromJson? (α := ResultLog) (← readJsonFile (dir / (name ++ ".json"))) with
    | .ok log => logs := logs.push log
    | .error err => throw <| .userError s!"{dir / name}.json: {err}"
  return logs.qsort fun a b => a.runAt < b.runAt

/-- The entries every run of this revision has produced, with a later run superseding an earlier. -/
def foldLogs (logs : Array ResultLog) : Array ResultEntry :=
  logs.foldl (init := #[]) fun entries log =>
    log.entries.foldl (init := entries) fun entries entry =>
      (entries.filter (·.test != entry.test)).push entry

/-- Every checker revision the store holds results for, folded into one log each. -/
def loadStore : IO (Array ResultLog) := do
  unless ← resultsDir.pathExists do return #[]
  let mut columns := #[]
  for checker in ← subdirectories resultsDir do
    for revision in ← subdirectories (resultsDir / checker) do
      let logs ← loadRevisionLogs checker revision
      let some newest := logs.back? | continue
      columns := columns.push { newest with entries := foldLogs logs }
  return columns

private def loadAll (α) [FromJson α] (dir : System.FilePath) (ext : String)
    (names : Array String) : IO (Array α) :=
  names.mapM fun name => do
    match fromJson? (α := α) (← readJsonFile (dir / (name ++ ext))) with
    | .ok value => return value
    | .error err => throw <| .userError s!"{name}{ext}: {err}"

def loadTestStats : IO (Array LocatedTest) := do
  let paths ← findNamesUnder builtTestsDir ".stats.json" 2
  let stats ← loadAll TestStats builtTestsDir ".stats.json" paths
  let located := stats.zipWith (fun s path =>
    ({ toTestStats := s, ndjsonPath := builtTestsDir / (path ++ ".ndjson") } : LocatedTest)) paths
  return located.qsort (·.name < ·.name)

end Arena
