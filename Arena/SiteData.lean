import Arena.Build
import Arena.Export

open Lean (Json ToJson FromJson toJson)

namespace Arena

def instructionsPerSecond : Nat := 6000000000

def tarballSizeLimit : Nat := 10 * 1024 * 1024

def benchmarkTest : String := "mathlib"
def baselineChecker : String := "official"

structure CheckerStats where
  acceptCorrect : Nat := 0
  acceptTotal : Nat := 0
  rejectCorrect : Nat := 0
  rejectTotal : Nat := 0
  declined : Nat := 0
  benchmark : Option Metrics := none
deriving Inhabited, ToJson, FromJson

def CheckerStats.record (stats : CheckerStats) (entry : ResultEntry)
    (expectation : Option Expectation) : CheckerStats :=
  let stats :=
    if entry.test == benchmarkTest && entry.status == .accepted then
      { stats with benchmark := some entry.toMetrics }
    else stats
  match entry.status, expectation with
  | .declined, _ | .error, _ => { stats with declined := stats.declined + 1 }
  | _, some .either => stats
  | status, some .accept =>
    { stats with
      acceptTotal := stats.acceptTotal + 1
      acceptCorrect := stats.acceptCorrect + if status == .accepted then 1 else 0 }
  | status, some .reject =>
    { stats with
      rejectTotal := stats.rejectTotal + 1
      rejectCorrect := stats.rejectCorrect + if status == .rejected then 1 else 0 }
  | _, none => stats

def expectationsOf (tests : Array LocatedTest) : Std.HashMap String (Option Expectation) :=
  tests.foldl (init := {}) fun map test => map.insert test.name test.expectation

def statsOfLog (tests : Array LocatedTest) (log : ResultLog) : CheckerStats :=
  let expectations := expectationsOf tests
  log.entries.foldl (init := {}) fun stats entry =>
    match Std.HashMap.get? expectations entry.test with
    | none => stats
    | some expectation => stats.record entry expectation

private def rankKey (stats : CheckerStats) : List Nat :=
  let instructions := (stats.benchmark.map (·.instructions)).getD 0
  [stats.rejectTotal - stats.rejectCorrect,
   stats.acceptTotal - stats.acceptCorrect,
   if instructions == 0 then 1 else 0,
   instructions,
   stats.declined]

structure TarballInfo where
  size : Nat
  goodCount : Nat
  badCount : Nat
deriving Inhabited, ToJson, FromJson

def createTarball (tests : Array LocatedTest) : IO TarballInfo := do
  let staging := buildDir / "tarball"
  removeIfExists staging
  let mut goodCount := 0
  let mut badCount := 0
  for test in tests do
    if test.size > tarballSizeLimit then continue
    unless ← test.ndjsonPath.pathExists do continue
    let some subdir := (match test.expectation with
      | some .accept => some "good"
      | some .reject => some "bad"
      | some .either | none => none) | continue
    if subdir == "good" then goodCount := goodCount + 1 else badCount := badCount + 1
    linkFile test.ndjsonPath (staging / subdir / (test.name ++ ".ndjson"))
  removeIfExists tarballPath
  let archive := (← absolute buildDir) / tarballName
  let args := #["-czhf", archive.toString, "good", "bad"]
  let outcome ← run { cmd := "tar", args, cwd := staging }
  unless outcome.ok do throw <| .userError s!"failed to build tarball: {outcome.stderr}"
  removeIfExists staging
  return { size := (← tarballPath.metadata).byteSize.toNat, goodCount, badCount }

structure CheckerInfo where
  name : String
  version : String
  declarationUrl : Option String
  sourceUrl : Option String
  stats : CheckerStats
deriving Inhabited, ToJson, FromJson

structure ResultInfo extends Metrics where
  checker : String
  test : String
  attempt : Attempt
deriving Inhabited, ToJson, FromJson

structure TarballData extends TarballInfo where
  name : String
deriving Inhabited, ToJson, FromJson

def ResultInfo.status (result : ResultInfo) : Status :=
  result.attempt.status

def ResultInfo.withoutProcessOutput (result : ResultInfo) : ResultInfo :=
  { result with attempt := result.attempt.withoutProcessOutput }

structure Payload where
  schemaVersion : Nat
  instructionsPerSecond : Nat
  benchmarkTest : String
  baselineChecker : String
  build : BuildInfo
  tarball : TarballData
  checkers : Array CheckerInfo
  tests : Array TestStats
  results : Array ResultInfo
deriving Inhabited, ToJson, FromJson

def Payload.withoutProcessOutput (payload : Payload) : Payload :=
  { payload with results := payload.results.map ResultInfo.withoutProcessOutput }

private def observedRate (entries : Array ResultEntry) : Option Float :=
  let (instructions, seconds) := entries.foldl (init := (0.0, 0.0)) fun (i, s) entry =>
    if entry.cpuTime > 0 && entry.instructions > 0 then
      (i + entry.instructions.toFloat, s + entry.cpuTime)
    else (i, s)
  if seconds > 0 then some (instructions / seconds) else none

def extractSources (tests : Array LocatedTest) : IO Unit := do
  let modules := tests.foldl (init := #[]) fun found test =>
    match test.sourceModule with
    | some module => if found.contains module then found else found.push module
    | none => found
  if modules.isEmpty then return
  removeIfExists siteSourcesDir
  IO.FS.createDirAll siteSourcesDir
  let build ← run { cmd := "lake", args := #["build", "Tests"] }
  unless build.ok do throw <| .userError s!"building the test library failed: {build.stderr}"
  for module in modules do
    let target := siteSourcesDir / (module ++ ".json")
    let outcome ← run {
      cmd := "lake"
      args := #["exe", "subverso-extract-mod", module, target.toString]
    }
    unless outcome.ok do
      throw <| .userError s!"highlighting {module} failed: {outcome.stderr}"
  IO.println s!"Extracted highlighted source for {modules.size} modules"

def renderedExportLimit : Nat := 128 * 1024

def renderExports (tests : Array LocatedTest) : IO Unit := do
  removeIfExists siteExportsDir
  let small := tests.filter fun test => test.size ≤ renderedExportLimit
  if small.isEmpty then return
  let env ← Export.coreEnvironment
  for test in small do
    unless ← test.ndjsonPath.pathExists do continue
    trace 1 s!"Rendering {test.name}..."
    match ← (Export.renderExport env test.ndjsonPath).toBaseIO with
    | .ok rendered => writeJsonFile (siteExportsDir / (test.name ++ ".json")) (toJson rendered)
    | .error err => note 1 s!"Could not render the export of {test.name}: {err}"
  IO.println s!"Rendered the exported declarations of {small.size} tests \
({tests.size - small.size} over {formatMemory renderedExportLimit.toFloat})"

structure CheckerColumn where
  log : ResultLog
  stats : CheckerStats

def rankColumns (columns : Array CheckerColumn) : Array CheckerColumn :=
  columns.qsort fun a b => compare (rankKey a.stats) (rankKey b.stats) |>.isLT

def currentColumns (tests : Array LocatedTest) : IO (Array CheckerColumn) := do
  return (← loadStore).map fun log => { log, stats := statsOfLog tests log }

def generateSiteData : IO Unit := do
  let info ← buildInfo
  let tests ← loadTestStats
  let columns ← currentColumns tests
  let entries := columns.flatMap (·.log.entries)
  match observedRate entries with
  | some rate => IO.println s!"Observed conversion rate: {formatUnitless rate}inst/s"
  | none => IO.println "No instruction counts available; using the fixed conversion rate"
  let ranked := rankColumns columns
  let tarball ← createTarball tests
  extractSources tests
  renderExports tests
  let payload : Payload := {
    schemaVersion := 1
    instructionsPerSecond
    benchmarkTest
    baselineChecker
    build := info
    tarball := { toTarballInfo := tarball, name := tarballName }
    checkers := ranked.map fun column =>
      { name := column.log.checker
        version := column.log.revision
        declarationUrl := none
        sourceUrl := if column.log.sourceUrl.isEmpty then none else some column.log.sourceUrl
        stats := column.stats }
    tests := tests.map (·.toTestStats)
    results := ranked.flatMap fun column =>
      column.log.entries.map fun entry =>
        { toMetrics := entry.toMetrics
          checker := column.log.checker
          test := entry.test
          attempt := entry.attempt }
  }
  writeJsonFile siteDataPath (toJson payload)
  IO.println s!"Wrote {siteDataPath} ({ranked.size} checker revisions, {tests.size} tests, \
{entries.size} results)"

end Arena
