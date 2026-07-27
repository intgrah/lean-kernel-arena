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

def TestStats.ndjsonPath (s : TestStats) : System.FilePath :=
  builtTestsDir / (s.name ++ ".ndjson")

private def field [ToJson α] (key : String) : Option α → List (String × Json)
  | some v => [(key, toJson v)]
  | none => []

instance : ToJson TestStats where
  toJson s := Json.mkObj <|
    [("name", toJson s.name),
     ("size", toJson s.size),
     ("lines", toJson s.lines),
     ("compare_perf", toJson s.comparePerf)]
    ++ field "outcome" (s.expectation.map ToString.toString)
    ++ field "description" s.generatedDescription
    ++ field "declaration_url" s.declarationUrl
    ++ field "source_url" s.sourceUrl
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
      generatedDescription := ← json.getObjValAs? (Option String) "description"
      declarationUrl := ← json.getObjValAs? (Option String) "declaration_url"
      sourceUrl := ← json.getObjValAs? (Option String) "source_url"
      exporterVersion := ← json.getObjValAs? (Option String) "lean4export_version"
      leanVersion := ← json.getObjValAs? (Option String) "lean_version"
    }

structure RunResult extends Metrics where
  checker : String
  test : String
  exitCode : Int
  stdout : String := ""
  stderr : String := ""
  message : Option String := none

def RunResult.status (result : RunResult) : Status :=
  Status.ofExitCode result.exitCode

def RunResult.correctness (result : RunResult) (expectation : Option Expectation) :
    Correctness :=
  Correctness.of result.status expectation

def RunResult.toJsonWith (r : RunResult) (expectation : Option Expectation) : Json :=
  Json.mkObj <| [
    ("checker", toJson r.checker),
    ("test", toJson r.test),
    ("status", toJson (ToString.toString r.status)),
    ("correctness", toJson (ToString.toString (r.correctness expectation))),
    ("exit_code", toJson r.exitCode),
    ("wall_time", toJson r.wallTime),
    ("cpu_time", toJson r.cpuTime),
    ("max_rss", toJson r.maxRss),
    ("instructions", toJson r.instructions),
    ("stdout", toJson r.stdout),
    ("stderr", toJson r.stderr)
  ] ++ field "message" r.message

instance : FromJson RunResult where
  fromJson? json := do
    return {
      checker := ← json.getObjValAs? String "checker"
      test := ← json.getObjValAs? String "test"
      exitCode := ← json.getObjValAs? Int "exit_code"
      wallTime := (← json.getObjValAs? (Option Float) "wall_time").getD 0
      cpuTime := (← json.getObjValAs? (Option Float) "cpu_time").getD 0
      maxRss := (json.getObjValAs? Nat "max_rss").toOption.getD 0
      instructions := (json.getObjValAs? Nat "instructions").toOption.getD 0
      stdout := (← json.getObjValAs? (Option String) "stdout").getD ""
      stderr := (← json.getObjValAs? (Option String) "stderr").getD ""
      message := ← json.getObjValAs? (Option String) "message"
    }

def resultFileName (checker test : String) : String :=
  s!"{checker}_{test.replace "/" "_"}.json"

def writeRunResult (result : RunResult) (expectation : Option Expectation) : IO Unit :=
  writeJsonFile (resultsDir / resultFileName result.checker result.test)
    (result.toJsonWith expectation)

private def loadAll (α) [FromJson α] (dir : System.FilePath) (ext : String)
    (names : Array String) : IO (Array α) :=
  names.mapM fun name => do
    match fromJson? (α := α) (← readJsonFile (dir / (name ++ ext))) with
    | .ok value => return value
    | .error err => throw <| .userError s!"{name}{ext}: {err}"

def loadTestStats : IO (Array TestStats) := do
  let stats ← loadAll TestStats builtTestsDir ".stats.json"
    (← findNamesUnder builtTestsDir ".stats.json" 2)
  return stats.qsort (·.name < ·.name)

def loadRunResults : IO (Array RunResult) := do
  loadAll RunResult resultsDir ".json" (← findNamesIn resultsDir ".json")

end Arena
