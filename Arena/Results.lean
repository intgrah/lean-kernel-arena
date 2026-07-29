import Arena.Config
import Arena.Proc

open Lean (Json ToJson FromJson toJson fromJson?)

namespace Arena

structure ExportMeta where
  exporterVersion : Option String := none
  leanVersion : Option String := none

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
  ndjsonPath : System.FilePath := ""


private def field [ToJson α] (key : String) : Option α → List (String × Json)
  | some v => [(key, toJson v)]
  | none => []

instance : ToJson TestStats where
  toJson s := Json.mkObj <|
    [("name", toJson s.name),
     ("size", toJson s.size),
     ("lines", toJson s.lines),
     ("compare_perf", toJson s.comparePerf),
     ("hash", toJson s.hash),
     ("recipe_hash", toJson s.recipeHash)]
    ++ field "outcome" (s.expectation.map ToString.toString)
    ++ field "description" s.generatedDescription
    ++ field "declaration_url" s.declarationUrl
    ++ field "source_url" s.sourceUrl
    ++ field "source_module" s.sourceModule
    ++ field "lean4export_version" s.exporterVersion
    ++ field "lean_version" s.leanVersion

instance : FromJson TestStats where
  fromJson? json := do
    return {
      name := ← json.getObjValAs? String "name"
      size := ← json.getObjValAs? Nat "size"
      lines := ← json.getObjValAs? Nat "lines"
      expectation := (← json.getObjValAs? (Option String) "outcome").bind Expectation.ofString?
      comparePerf := (← json.getObjValAs? (Option Bool) "compare_perf").getD false
      hash := (← json.getObjValAs? (Option String) "hash").getD ""
      recipeHash := (← json.getObjValAs? (Option String) "recipe_hash").getD ""
      generatedDescription := ← json.getObjValAs? (Option String) "description"
      declarationUrl := ← json.getObjValAs? (Option String) "declaration_url"
      sourceUrl := ← json.getObjValAs? (Option String) "source_url"
      sourceModule := ← json.getObjValAs? (Option String) "source_module"
      exporterVersion := ← json.getObjValAs? (Option String) "lean4export_version"
      leanVersion := ← json.getObjValAs? (Option String) "lean_version"
    }

structure ResultEntry extends Metrics where
  test : String
  testHash : String
  attempt : Attempt
  runAt : String

def ResultEntry.status (entry : ResultEntry) : Status :=
  entry.attempt.status

def ResultEntry.correctness (entry : ResultEntry) (expectation : Option Expectation) :
    Correctness :=
  Correctness.of entry.status expectation

instance : ToJson ResultEntry where
  toJson e := Json.mkObj [
    ("test", toJson e.test),
    ("test_hash", toJson e.testHash),
    ("attempt", toJson e.attempt),
    ("wall_time", toJson e.wallTime),
    ("cpu_time", toJson e.cpuTime),
    ("max_rss", toJson e.maxRss),
    ("instructions", toJson e.instructions),
    ("run_at", toJson e.runAt)
  ]

instance : FromJson ResultEntry where
  fromJson? json := do
    return {
      test := ← json.getObjValAs? String "test"
      testHash := ← json.getObjValAs? String "test_hash"
      attempt := ← json.getObjValAs? Attempt "attempt"
      wallTime := (← json.getObjValAs? (Option Float) "wall_time").getD 0
      cpuTime := (← json.getObjValAs? (Option Float) "cpu_time").getD 0
      maxRss := (json.getObjValAs? Nat "max_rss").toOption.getD 0
      instructions := (json.getObjValAs? Nat "instructions").toOption.getD 0
      runAt := ← json.getObjValAs? String "run_at"
    }

structure ResultLog where
  checker : String
  revision : String
  recipeHash : String
  runner : String
  entries : Array ResultEntry
deriving Inhabited

instance : ToJson ResultLog where
  toJson log := Json.mkObj [
    ("checker", toJson log.checker),
    ("revision", toJson log.revision),
    ("recipe_hash", toJson log.recipeHash),
    ("runner", toJson log.runner),
    ("entries", toJson log.entries)
  ]

instance : FromJson ResultLog where
  fromJson? json := do
    return {
      checker := ← json.getObjValAs? String "checker"
      revision := ← json.getObjValAs? String "revision"
      recipeHash := ← json.getObjValAs? String "recipe_hash"
      runner := ← json.getObjValAs? String "runner"
      entries := ← json.getObjValAs? (Array ResultEntry) "entries"
    }

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

def loadTestStats : IO (Array TestStats) := do
  let paths ← findNamesUnder builtTestsDir ".stats.json" 2
  let stats ← loadAll TestStats builtTestsDir ".stats.json" paths
  let located := stats.zipWith (fun s path =>
    { s with ndjsonPath := builtTestsDir / (path ++ ".ndjson") }) paths
  return located.qsort (·.name < ·.name)

def loadResultLogs : IO (Array ResultLog) := do
  loadAll ResultLog resultsDir ".json" (← findNamesUnder resultsDir ".json" 2)

end Arena
