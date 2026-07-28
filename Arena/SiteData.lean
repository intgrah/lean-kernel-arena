import Arena.Run

open Lean (Json toJson)

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

def CheckerStats.record (stats : CheckerStats) (result : RunResult)
    (expectation : Option Expectation) : CheckerStats :=
  let stats :=
    if result.test == benchmarkTest && result.status == .accepted then
      { stats with benchmark := some result.toMetrics }
    else stats
  match result.status, expectation with
  | .declined, _ | .error, _ => { stats with declined := stats.declined + 1 }
  | status, some .accept =>
    { stats with
      acceptTotal := stats.acceptTotal + 1
      acceptCorrect := stats.acceptCorrect + if status == .accepted then 1 else 0 }
  | status, some .reject =>
    { stats with
      rejectTotal := stats.rejectTotal + 1
      rejectCorrect := stats.rejectCorrect + if status == .rejected then 1 else 0 }
  | _, none => stats

def computeCheckerStats (tests : Array TestStats) (results : Array RunResult) :
    Std.HashMap String CheckerStats :=
  let expectations := expectationsOf tests
  results.foldl (init := {}) fun stats result =>
    match Std.HashMap.get? expectations result.test with
    | none => stats
    | some expectation =>
      let current := (Std.HashMap.get? stats result.checker).getD {}
      stats.insert result.checker (current.record result expectation)

private def rankKey (stats : CheckerStats) : List Nat :=
  let instructions := (stats.benchmark.map (·.instructions)).getD 0
  [stats.rejectTotal - stats.rejectCorrect,
   stats.acceptTotal - stats.acceptCorrect,
   if instructions == 0 then 1 else 0,
   instructions,
   stats.declined]

def rankCheckers (checkers : Array (CheckerConfig × CheckerStats)) :
    Array (CheckerConfig × CheckerStats) :=
  checkers.qsort fun a b => compare (rankKey a.2) (rankKey b.2) |>.isLT

structure TarballInfo where
  size : Nat
  goodCount : Nat
  badCount : Nat

def createTarball (tests : Array TestStats) : IO TarballInfo := do
  let staging := buildDir / "tarball"
  removeIfExists staging
  let mut goodCount := 0
  let mut badCount := 0
  for test in tests do
    if test.size > tarballSizeLimit then continue
    unless ← test.ndjsonPath.pathExists do continue
    let accepted := test.expectation == some .accept
    if accepted then goodCount := goodCount + 1 else badCount := badCount + 1
    let subdir := if accepted then "good" else "bad"
    linkFile test.ndjsonPath (staging / subdir / (test.name ++ ".ndjson"))
  removeIfExists tarballPath
  let archive := (← absolute buildDir) / tarballName
  let args := #["-czhf", archive.toString, "good", "bad"]
  let outcome ← run { cmd := "tar", args, cwd := staging }
  unless outcome.ok do throw <| .userError s!"failed to build tarball: {outcome.stderr}"
  removeIfExists staging
  return { size := (← tarballPath.metadata).byteSize.toNat, goodCount, badCount }

private def nanos (seconds : Float) : Nat :=
  if seconds > 0 then (seconds * 1e9).toUInt64.toNat else 0

private def metricsFields (metrics : Metrics) : List (String × Json) :=
  [("wall_nanos", toJson (nanos metrics.wallTime)),
   ("cpu_nanos", toJson (nanos metrics.cpuTime)),
   ("max_rss", toJson metrics.maxRss),
   ("instructions", toJson metrics.instructions)]

private def checkerJson (config : CheckerConfig) (stats : CheckerStats)
    (links : SourceLinks) : Json :=
  Json.mkObj [
    ("name", toJson config.name),
    ("version", toJson config.displayVersion),
    ("serious", toJson config.serious),
    ("declaration_url", toJson links.declarationUrl),
    ("source_url", toJson links.sourceUrl),
    ("stats", Json.mkObj [
      ("accept_correct", toJson stats.acceptCorrect),
      ("accept_total", toJson stats.acceptTotal),
      ("reject_correct", toJson stats.rejectCorrect),
      ("reject_total", toJson stats.rejectTotal),
      ("declined", toJson stats.declined),
      ("benchmark", toJson (stats.benchmark.map (Json.mkObj <| metricsFields ·)))
    ])
  ]

private def resultJson (result : RunResult) : Json :=
  Json.mkObj <| [
    ("checker", toJson result.checker),
    ("test", toJson result.test),
    ("exit_code", toJson result.exitCode),
    ("stdout", toJson result.stdout),
    ("stderr", toJson result.stderr)
  ] ++ metricsFields result.toMetrics

private def buildJson (info : BuildInfo) : Json :=
  Json.mkObj [
    ("timestamp", toJson info.timestamp),
    ("short_revision", toJson info.shortRevision),
    ("commit_url", toJson info.commitUrl),
    ("action_url", toJson info.actionUrl),
    ("action_run_id", toJson info.actionRunId)
  ]

private def observedRate (results : Array RunResult) : Option Float :=
  let (instructions, seconds) := results.foldl (init := (0.0, 0.0)) fun (i, s) result =>
    if result.cpuTime > 0 && result.instructions > 0 then
      (i + result.instructions.toFloat, s + result.cpuTime)
    else (i, s)
  if seconds > 0 then some (instructions / seconds) else none

def extractSources (tests : Array TestStats) : IO Unit := do
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

def generateSiteData : IO Unit := do
  let info ← buildInfo
  let configs ← loadCheckerConfigs
  let tests ← loadTestStats
  let results ← loadRunResults
  match observedRate results with
  | some rate => IO.println s!"Observed conversion rate: {formatUnitless rate}inst/s"
  | none => IO.println "No instruction counts available; using the fixed conversion rate"
  let stats := computeCheckerStats tests results
  let ranked := rankCheckers <| configs.map fun config =>
    (config, (Std.HashMap.get? stats config.name).getD {})
  let tarball ← createTarball tests
  extractSources tests
  let payload := Json.mkObj [
    ("schema_version", toJson 1),
    ("instructions_per_second", toJson instructionsPerSecond),
    ("benchmark_test", toJson benchmarkTest),
    ("baseline_checker", toJson baselineChecker),
    ("build", buildJson info),
    ("tarball", Json.mkObj [
      ("name", toJson tarballName),
      ("size", toJson tarball.size),
      ("good_count", toJson tarball.goodCount),
      ("bad_count", toJson tarball.badCount)
    ]),
    ("checkers", Json.arr <| ranked.map fun (config, stats) =>
      checkerJson config stats (config.sourceLinks info.gitRevision)),
    ("tests", Json.arr <| tests.map toJson),
    ("results", Json.arr <| results.map resultJson)
  ]
  writeJsonFile siteDataPath payload
  IO.println s!"Wrote {siteDataPath} ({ranked.size} checkers, {tests.size} tests, \
{results.size} results)"

end Arena
