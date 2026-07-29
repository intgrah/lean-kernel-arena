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
  entries : Array ResultEntry
deriving Inhabited, ToJson, FromJson

inductive Stored
  | none
  | matching (log : ResultLog)
  | otherRecipe (log : ResultLog)

def Stored.log? : Stored → Option ResultLog
  | .none => Option.none
  | .matching log | .otherRecipe log => some log

def ResultLog.find? (log : ResultLog) (test : String) : Option ResultEntry :=
  log.entries.find? (·.test == test)

def ResultLog.record (log : ResultLog) (entry : ResultEntry) : ResultLog :=
  { log with
    entries := (log.entries.filter (·.test != entry.test)).push entry
      |>.qsort (·.test < ·.test) }

def resultLogPath (checker revision : String) : System.FilePath :=
  resultsDir / checker / (revision ++ ".json")

def loadResultLog (checker revision recipeHash : String) : IO Stored := do
  let path := resultLogPath checker revision
  unless ← path.pathExists do return .none
  match fromJson? (α := ResultLog) (← readJsonFile path) with
  | .ok log => return if log.recipeHash == recipeHash then .matching log else .otherRecipe log
  | .error err => throw <| .userError s!"{path}: {err}"

def writeResultLog (log : ResultLog) : IO Unit :=
  writeJsonFile (resultLogPath log.checker log.revision) (toJson log)

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

def loadResultLogs : IO (Array ResultLog) := do
  loadAll ResultLog resultsDir ".json" (← findNamesUnder resultsDir ".json" 2)

end Arena
