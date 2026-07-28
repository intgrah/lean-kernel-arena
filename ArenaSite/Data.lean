import Lean.Data.Json
import SubVerso.Module
import VersoBlog
import Arena.Layout
import Arena.Util

open Lean
open Lean.Elab Term

namespace ArenaSite

open Arena (Expectation Status)
export Arena (Expectation Status)

deriving instance Quote for Arena.Expectation
deriving instance Quote for Arena.Status

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
  benchmark : Option Metrics
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
  sourceModule : Option String
deriving Repr, Inhabited, Quote

structure ResultInfo where
  checker : String
  test : String
  exitCode : Int
  metrics : Metrics
  stdout : String
  stderr : String
deriving Repr, Inhabited, Quote

def ResultInfo.status (result : ResultInfo) : Status :=
  Status.ofExitCode result.exitCode

def ResultInfo.withoutProcessOutput (result : ResultInfo) : ResultInfo :=
  { result with stdout := "", stderr := "" }

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
  benchmarkTest : String
  baselineChecker : String
  build : BuildInfo
  tarball : TarballInfo
  checkers : Array CheckerInfo
  tests : Array TestInfo
  results : Array ResultInfo
deriving Inhabited, Quote

def Payload.withoutProcessOutput (payload : Payload) : Payload :=
  { payload with results := payload.results.map (·.withoutProcessOutput) }

instance : FromJson Metrics where
  fromJson? json := do
    return {
      wallNanos := (← json.getObjValAs? (Option Nat) "wall_nanos").getD 0
      cpuNanos := (← json.getObjValAs? (Option Nat) "cpu_nanos").getD 0
      maxRss := (← json.getObjValAs? (Option Nat) "max_rss").getD 0
      instructions := (← json.getObjValAs? (Option Nat) "instructions").getD 0
    }

instance : FromJson CheckerStats where
  fromJson? json := do
    return {
      acceptCorrect := ← json.getObjValAs? Nat "accept_correct"
      acceptTotal := ← json.getObjValAs? Nat "accept_total"
      rejectCorrect := ← json.getObjValAs? Nat "reject_correct"
      rejectTotal := ← json.getObjValAs? Nat "reject_total"
      declined := ← json.getObjValAs? Nat "declined"
      benchmark := ← json.getObjValAs? (Option Metrics) "benchmark"
    }

instance : FromJson CheckerInfo where
  fromJson? json := do
    return {
      name := ← json.getObjValAs? String "name"
      version := ← json.getObjValAs? String "version"
      serious := (← json.getObjValAs? (Option Bool) "serious").getD false
      declarationUrl := ← json.getObjValAs? (Option String) "declaration_url"
      sourceUrl := ← json.getObjValAs? (Option String) "source_url"
      stats := ← json.getObjValAs? CheckerStats "stats"
    }

instance : FromJson TestInfo where
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
      leanVersion := ← json.getObjValAs? (Option String) "lean_version"
      exporterVersion := ← json.getObjValAs? (Option String) "lean4export_version"
      sourceModule := ← json.getObjValAs? (Option String) "source_module"
    }

instance : FromJson ResultInfo where
  fromJson? json := do
    return {
      checker := ← json.getObjValAs? String "checker"
      test := ← json.getObjValAs? String "test"
      exitCode := ← json.getObjValAs? Int "exit_code"
      metrics := ← FromJson.fromJson? json
      stdout := (← json.getObjValAs? (Option String) "stdout").getD ""
      stderr := (← json.getObjValAs? (Option String) "stderr").getD ""
    }

instance : FromJson BuildInfo where
  fromJson? json := do
    return {
      timestamp := ← json.getObjValAs? String "timestamp"
      shortRevision := ← json.getObjValAs? (Option String) "short_revision"
      commitUrl := ← json.getObjValAs? (Option String) "commit_url"
      actionUrl := ← json.getObjValAs? (Option String) "action_url"
      actionRunId := ← json.getObjValAs? (Option String) "action_run_id"
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
      benchmarkTest := ← json.getObjValAs? String "benchmark_test"
      baselineChecker := ← json.getObjValAs? String "baseline_checker"
      build := ← json.getObjValAs? BuildInfo "build"
      tarball := ← json.getObjValAs? TarballInfo "tarball"
      checkers := ← json.getObjValAs? (Array CheckerInfo) "checkers"
      tests := ← json.getObjValAs? (Array TestInfo) "tests"
      results := ← json.getObjValAs? (Array ResultInfo) "results"
    }

def expectedSchemaVersion : Nat := 1

initialize payloadCache : IO.Ref (Option Payload) ← IO.mkRef none

private def readPayload : TermElabM Payload := do
  let path := Arena.siteDataPath
  let json ← match Json.parse (← IO.FS.readFile path) with
    | .ok json => pure json
    | .error err => throwError "failed to parse {path}: {err}"
  let payload ← match FromJson.fromJson? (α := Payload) json with
    | .ok payload => pure payload
    | .error err => throwError "failed to decode {path}: {err}"
  unless payload.schemaVersion == expectedSchemaVersion do
    throwError "{path} has schema version {payload.schemaVersion}, expected \
{expectedSchemaVersion}; regenerate it with `lka site-data`"
  return payload

def loadPayload : TermElabM Payload := do
  if let some cached := ← payloadCache.get then return cached
  let payload ← readPayload
  payloadCache.set (some payload)
  return payload

def loadModuleSource (module : String) : TermElabM SubVerso.Module.Module := do
  let path := Arena.siteSourcesDir / (module ++ ".json")
  let json ← match Json.parse (← IO.FS.readFile path) with
    | .ok json => pure json
    | .error err => throwError "failed to parse {path}: {err}"
  match FromJson.fromJson? (α := SubVerso.Module.Module) json with
  | .ok mod => return mod
  | .error err => throwError "failed to decode {path}: {err}"

abbrev ResultIndex := Arena.PairIndex ResultInfo

def indexOf (payload : Payload) : ResultIndex :=
  Arena.indexPairs (fun result => (result.checker, result.test)) payload.results

def findResult (index : ResultIndex) (checker test : String) : Option ResultInfo :=
  Std.HashMap.get? index (checker, test)

end ArenaSite
