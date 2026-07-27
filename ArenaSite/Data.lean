import Lean.Data.Json
import VersoBlog

open Lean
open Lean.Elab Term

namespace ArenaSite

inductive Expectation
  | accept
  | reject
deriving DecidableEq, Repr, Inhabited, Quote

def Expectation.ofString? : String → Option Expectation
  | "accept" => some .accept
  | "reject" => some .reject
  | _ => none

inductive Status
  | accepted
  | rejected
  | declined
  | error
deriving DecidableEq, Repr, Inhabited, Quote

def Status.ofString? : String → Option Status
  | "accepted" => some .accepted
  | "rejected" => some .rejected
  | "declined" => some .declined
  | "error" => some .error
  | _ => none

def Status.satisfies : Status → Option Expectation → Bool
  | .accepted, some .accept => true
  | .rejected, some .reject => true
  | _, _ => false

def Status.isInconclusive : Status → Bool
  | .declined | .error => true
  | _ => false

structure Metrics where
  wallNanos : Nat
  cpuNanos : Nat
  maxRss : Nat
  instructions : Nat
deriving Repr, Inhabited, Quote

def Metrics.wallSeconds (metrics : Metrics) : Float := metrics.wallNanos.toFloat / 1e9

def Metrics.cpuSeconds (metrics : Metrics) : Float := metrics.cpuNanos.toFloat / 1e9

structure CheckerStats where
  acceptCorrect : Nat
  acceptTotal : Nat
  rejectCorrect : Nat
  rejectTotal : Nat
  declined : Nat
  mathlib : Option Metrics
deriving Repr, Inhabited, Quote

structure CheckerInfo where
  name : String
  version : String
  serious : Bool
  declarationUrl : Option String
  sourceUrl : Option String
  stats : CheckerStats
deriving Repr, Inhabited, Quote

structure TestInfo where
  name : String
  size : Nat
  lines : Nat
  expectation : Option Expectation
  comparePerf : Bool
  generatedDescription : Option String
  declarationUrl : Option String
  sourceUrl : Option String
  leanVersion : Option String
  exporterVersion : Option String
deriving Repr, Inhabited, Quote

structure ResultInfo where
  checker : String
  test : String
  status : Status
  exitCode : Int
  metrics : Metrics
  stdout : String
  stderr : String
deriving Repr, Inhabited, Quote

structure BuildInfo where
  timestamp : String
  shortRevision : Option String
  commitUrl : Option String
  actionUrl : Option String
  actionRunId : Option String
deriving Repr, Inhabited, Quote

structure TarballInfo where
  name : String
  size : Nat
  goodCount : Nat
  badCount : Nat
deriving Repr, Inhabited, Quote

structure Payload where
  schemaVersion : Nat
  instructionsPerSecond : Nat
  build : BuildInfo
  tarball : TarballInfo
  checkers : Array CheckerInfo
  tests : Array TestInfo
  results : Array ResultInfo
deriving Inhabited, Quote

def Payload.withoutProcessOutput (payload : Payload) : Payload :=
  { payload with
    results := payload.results.map fun result => { result with stdout := "", stderr := "" } }

private def optString (json : Json) (key : String) : Option String :=
  (json.getObjValAs? String key).toOption

private def optBool (json : Json) (key : String) : Bool :=
  (json.getObjValAs? Bool key).toOption.getD false

instance : FromJson Metrics where
  fromJson? json := do
    return {
      wallNanos := (json.getObjValAs? Nat "wall_nanos").toOption.getD 0
      cpuNanos := (json.getObjValAs? Nat "cpu_nanos").toOption.getD 0
      maxRss := (json.getObjValAs? Nat "max_rss").toOption.getD 0
      instructions := (json.getObjValAs? Nat "instructions").toOption.getD 0
    }

instance : FromJson CheckerStats where
  fromJson? json := do
    return {
      acceptCorrect := ← json.getObjValAs? Nat "accept_correct"
      acceptTotal := ← json.getObjValAs? Nat "accept_total"
      rejectCorrect := ← json.getObjValAs? Nat "reject_correct"
      rejectTotal := ← json.getObjValAs? Nat "reject_total"
      declined := ← json.getObjValAs? Nat "declined"
      mathlib := (json.getObjValAs? Metrics "mathlib").toOption
    }

instance : FromJson CheckerInfo where
  fromJson? json := do
    return {
      name := ← json.getObjValAs? String "name"
      version := ← json.getObjValAs? String "version"
      serious := optBool json "serious"
      declarationUrl := optString json "declaration_url"
      sourceUrl := optString json "source_url"
      stats := ← json.getObjValAs? CheckerStats "stats"
    }

instance : FromJson TestInfo where
  fromJson? json := do
    return {
      name := ← json.getObjValAs? String "name"
      size := ← json.getObjValAs? Nat "size"
      lines := ← json.getObjValAs? Nat "lines"
      expectation := (optString json "outcome").bind Expectation.ofString?
      comparePerf := optBool json "compare_perf"
      generatedDescription := optString json "description"
      declarationUrl := optString json "declaration_url"
      sourceUrl := optString json "source_url"
      leanVersion := optString json "lean_version"
      exporterVersion := optString json "lean4export_version"
    }

instance : FromJson ResultInfo where
  fromJson? json := do
    let statusText ← json.getObjValAs? String "status"
    let some status := Status.ofString? statusText
      | .error s!"unknown status: {statusText}"
    return {
      checker := ← json.getObjValAs? String "checker"
      test := ← json.getObjValAs? String "test"
      status
      exitCode := ← json.getObjValAs? Int "exit_code"
      metrics := ← FromJson.fromJson? json
      stdout := (optString json "stdout").getD ""
      stderr := (optString json "stderr").getD ""
    }

instance : FromJson BuildInfo where
  fromJson? json := do
    return {
      timestamp := ← json.getObjValAs? String "timestamp"
      shortRevision := optString json "short_revision"
      commitUrl := optString json "commit_url"
      actionUrl := optString json "action_url"
      actionRunId := optString json "action_run_id"
    }

instance : FromJson TarballInfo where
  fromJson? json := do
    return {
      name := ← json.getObjValAs? String "name"
      size := ← json.getObjValAs? Nat "size"
      goodCount := ← json.getObjValAs? Nat "good_count"
      badCount := ← json.getObjValAs? Nat "bad_count"
    }

instance : FromJson Payload where
  fromJson? json := do
    return {
      schemaVersion := ← json.getObjValAs? Nat "schema_version"
      instructionsPerSecond := ← json.getObjValAs? Nat "instructions_per_second"
      build := ← json.getObjValAs? BuildInfo "build"
      tarball := ← json.getObjValAs? TarballInfo "tarball"
      checkers := ← json.getObjValAs? (Array CheckerInfo) "checkers"
      tests := ← json.getObjValAs? (Array TestInfo) "tests"
      results := ← json.getObjValAs? (Array ResultInfo) "results"
    }

def siteDataPath : System.FilePath := "site-data/arena.json"

def expectedSchemaVersion : Nat := 1

def loadPayload : TermElabM Payload := do
  let raw ← IO.FS.readFile siteDataPath
  let json ← match Json.parse raw with
    | .ok json => pure json
    | .error err => throwError "failed to parse {siteDataPath}: {err}"
  let payload ← match FromJson.fromJson? (α := Payload) json with
    | .ok payload => pure payload
    | .error err => throwError "failed to decode {siteDataPath}: {err}"
  unless payload.schemaVersion == expectedSchemaVersion do
    throwError "{siteDataPath} has schema version {payload.schemaVersion}, \
expected {expectedSchemaVersion}; regenerate it with `lka site-data`"
  return payload

end ArenaSite
