import Arena.Config
import Arena.Proc

open Lean (Json ToJson FromJson toJson fromJson?)

namespace Arena

def buildDir : System.FilePath := "_build"
def builtTestsDir : System.FilePath := buildDir / "tests"
def builtCheckersDir : System.FilePath := buildDir / "checkers"
def resultsDir : System.FilePath := "_results"
def siteDataPath : System.FilePath := "site-data/arena.json"

structure ExportMeta where
  exporterVersion : Option String := none
  leanVersion : Option String := none
  leanGithash : Option String := none

structure TestStats extends ExportMeta where
  name : String
  size : Nat
  lines : Nat
  expectation : Option Expectation
  comparePerf : Bool := false
  skipOnCi : Bool := false
  generatedDescription : Option String := none
  configFile : String
  declarationUrl : Option String := none
  sourceUrl : Option String := none

def TestStats.ndjsonPath (s : TestStats) : System.FilePath :=
  builtTestsDir / (s.name ++ ".ndjson")

private def field [ToJson α] (key : String) : Option α → List (String × Json)
  | some v => [(key, toJson v)]
  | none => []

private def flagField (key : String) (set : Bool) : List (String × Json) :=
  if set then [(key, toJson true)] else []

instance : ToJson TestStats where
  toJson s := Json.mkObj <|
    [("name", toJson s.name),
     ("size", toJson s.size),
     ("lines", toJson s.lines),
     ("config_file", toJson s.configFile)]
    ++ field "outcome" (s.expectation.map toString)
    ++ flagField "compare_perf" s.comparePerf
    ++ flagField "skip_on_ci" s.skipOnCi
    ++ field "description" s.generatedDescription
    ++ field "declaration_url" s.declarationUrl
    ++ field "source_url" s.sourceUrl
    ++ field "lean4export_version" s.exporterVersion
    ++ field "lean_version" s.leanVersion
    ++ field "lean_githash" s.leanGithash

private def optString (json : Json) (key : String) : Option String :=
  (json.getObjValAs? String key).toOption

private def optBool (json : Json) (key : String) : Bool :=
  (json.getObjValAs? Bool key).toOption.getD false

instance : FromJson TestStats where
  fromJson? json := do
    return {
      name := ← json.getObjValAs? String "name"
      size := ← json.getObjValAs? Nat "size"
      lines := ← json.getObjValAs? Nat "lines"
      configFile := (optString json "config_file").getD ""
      expectation := (optString json "outcome").bind Expectation.ofString?
      comparePerf := optBool json "compare_perf"
      skipOnCi := optBool json "skip_on_ci"
      generatedDescription := optString json "description"
      declarationUrl := optString json "declaration_url"
      sourceUrl := optString json "source_url"
      exporterVersion := optString json "lean4export_version"
      leanVersion := optString json "lean_version"
      leanGithash := optString json "lean_githash"
    }

inductive Status
  | accepted
  | rejected
  | declined
  | error
deriving DecidableEq, Repr

def Status.ofExitCode : UInt32 → Status
  | 0 => .accepted
  | 1 => .rejected
  | 2 => .declined
  | _ => .error

def Status.toString : Status → String
  | .accepted => "accepted"
  | .rejected => "rejected"
  | .declined => "declined"
  | .error => "error"

def Status.ofString? : String → Option Status
  | "accepted" => some .accepted
  | "rejected" => some .rejected
  | "declined" => some .declined
  | "error" => some .error
  | _ => none

instance : ToString Status := ⟨Status.toString⟩

def Status.matches : Status → Expectation → Bool
  | .accepted, .accept => true
  | .rejected, .reject => true
  | _, _ => false

inductive Correctness
  | correct
  | incorrect
  | declined
  | error
deriving DecidableEq, Repr

def Correctness.toString : Correctness → String
  | .correct => "correct"
  | .incorrect => "incorrect"
  | .declined => "declined"
  | .error => "error"

def Correctness.ofString? : String → Option Correctness
  | "correct" => some .correct
  | "incorrect" => some .incorrect
  | "declined" => some .declined
  | "error" => some .error
  | _ => none

instance : ToString Correctness := ⟨Correctness.toString⟩

def Correctness.of (status : Status) (expectation : Option Expectation) : Correctness :=
  match status, expectation with
  | .declined, _ => .declined
  | .error, _ => .error
  | status, some expectation => if status.matches expectation then .correct else .incorrect
  | _, none => .error

structure RunResult extends Metrics where
  checker : String
  test : String
  status : Status
  correctness : Correctness
  exitCode : Int
  stdout : String := ""
  stderr : String := ""
  message : Option String := none

instance : ToJson RunResult where
  toJson r := Json.mkObj <| [
    ("checker", toJson r.checker),
    ("test", toJson r.test),
    ("status", toJson (toString r.status)),
    ("correctness", toJson (toString r.correctness)),
    ("exit_code", toJson r.exitCode),
    ("wall_time", toJson r.wallTime),
    ("cpu_time", toJson r.cpuTime),
    ("max_rss", toJson r.maxRss),
    ("instructions", toJson r.instructions),
    ("stdout", toJson r.stdout),
    ("stderr", toJson r.stderr)
  ] ++ field "message" r.message

private def optFloat (json : Json) (key : String) : Float :=
  (json.getObjValAs? Float key).toOption.getD 0

instance : FromJson RunResult where
  fromJson? json := do
    let statusText ← json.getObjValAs? String "status"
    let some status := Status.ofString? statusText
      | .error s!"unknown status: {statusText}"
    let correctnessText ← json.getObjValAs? String "correctness"
    let some correctness := Correctness.ofString? correctnessText
      | .error s!"unknown correctness: {correctnessText}"
    return {
      checker := ← json.getObjValAs? String "checker"
      test := ← json.getObjValAs? String "test"
      status, correctness
      exitCode := ← json.getObjValAs? Int "exit_code"
      wallTime := optFloat json "wall_time"
      cpuTime := optFloat json "cpu_time"
      maxRss := (json.getObjValAs? Nat "max_rss").toOption.getD 0
      instructions := (json.getObjValAs? Nat "instructions").toOption.getD 0
      stdout := (optString json "stdout").getD ""
      stderr := (optString json "stderr").getD ""
      message := optString json "message"
    }

def resultFileName (checker test : String) : String :=
  s!"{checker}_{test.replace "/" "_"}.json"

def writeRunResult (result : RunResult) : IO Unit :=
  writeJsonFile (resultsDir / resultFileName result.checker result.test) (toJson result)

def loadTestStats : IO (Array TestStats) := do
  let names ← findNamesUnder builtTestsDir ".stats.json"
  let mut stats := #[]
  for name in names do
    let json ← readJsonFile (builtTestsDir / (name ++ ".stats.json"))
    match fromJson? (α := TestStats) json with
    | .ok s => stats := stats.push s
    | .error err => throw <| .userError s!"{name}.stats.json: {err}"
  return stats.qsort (·.name < ·.name)

def loadRunResults : IO (Array RunResult) := do
  let names ← findNamesIn resultsDir ".json"
  let mut results := #[]
  for name in names do
    let json ← readJsonFile (resultsDir / (name ++ ".json"))
    match fromJson? (α := RunResult) json with
    | .ok r => results := results.push r
    | .error err => throw <| .userError s!"{name}.json: {err}"
  return results

end Arena
