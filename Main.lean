import Cli
import Arena.SiteData

open Arena Cli

def applyVerbosity (p : Parsed) : IO Unit :=
  verboseRef.set (p.hasFlag "verbose")

def globsOf (p : Parsed) (flag : String) : Array String :=
  match p.flag? flag with
  | some value => value.as! (Array String)
  | none => #[]

def select (name : α → String) (kind : String) (patterns : Array String)
    (items : Array α) : IO (Array α) := do
  let chosen := selectByPatterns name patterns items
  if chosen.isEmpty && !patterns.isEmpty then
    throw <| .userError s!"no {kind} match {" ".intercalate patterns.toList}"
  return chosen

def forEachReporting (items : Array α) (act : α → IO Unit) : IO UInt32 := do
  let mut failed := 0
  for item in items do
    try
      act item
    catch error =>
      IO.eprintln s!"  Error: {error}"
      failed := failed + 1
  IO.println s!"\nResults: {items.size - failed} succeeded, {failed} failed"
  return if failed == 0 then 0 else 1

def runBuildTest (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  let revision := (← buildInfo).gitRevision
  let configs ← select TestConfig.name "tests" (p.variableArgsAs! String) (← loadTestConfigs)
  let configs :=
    if p.hasFlag "skip-ci" then configs.filter (!·.skipOnCi) else configs
  let stale ←
    if p.hasFlag "force" then pure configs else configs.filterM (fun c => return !(← isCurrent c))
  let current := configs.size - stale.size
  if current > 0 then
    IO.println s!"{current} test(s) already match their inputs; rebuilding {stale.size}"
  forEachReporting stale fun config => buildTest config revision

structure Target where
  checker : CheckerConfig
  pin : Pin

def Target.name (target : Target) : String :=
  s!"{target.checker.name}@{target.pin.id}"

def selectTargets (p : Parsed) : IO (Array Target) := do
  let configs ← select CheckerConfig.name "checkers" (globsOf p "checker") (← loadCheckerConfigs)
  match p.flag? "pin" with
  | none => return configs.map fun checker => { checker, pin := checker.pin }
  | some value =>
    let some checker := configs[0]? | throw <| .userError "`--pin` needs a checker"
    if configs.size != 1 then
      throw <| .userError "`--pin` applies to one checker; name it with `--checker`"
    let pin := value.as! String
    return #[{ checker, pin := match checker.source with
      | .git .. => .commit pin
      | _ => .toolchain pin }]

def runBuildChecker (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  let targets ← selectTargets p
  forEachReporting targets fun target => buildChecker target.checker target.pin

def runRun (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  let targets ← selectTargets p
  let tests ← select TestStats.name "tests" (globsOf p "test") (← loadTestStats)
  if targets.isEmpty then
    IO.println "No checkers selected."
    return 0
  if tests.isEmpty then
    IO.println "No built tests found."
    return 0
  let mut produced := #[]
  for target in targets do
    produced := produced ++
      (← runRevision target.checker target.pin tests (p.hasFlag "force"))
  reportTally tests produced
  return 0

def runSiteData (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  generateSiteData
  return 0

def runBuildSite (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  let outdir : System.FilePath := p.flag! "outdir" |>.as! String
  generateSiteData
  IO.FS.createDirAll outdir
  let site ← run {
    cmd := "lake"
    args := #["exe", "arena-site", "--output", outdir.toString]
    captureOutput := false
  }
  unless site.ok do throw <| .userError "site generation failed"
  copyFile tarballPath (outdir / tarballName)
  IO.println s!"Generated: {outdir / tarballName}"
  IO.println s!"\nSite built successfully in: {outdir}"
  return 0

def buildTestCmd := `[Cli|
  "build-test" VIA runBuildTest;
  "Build the .ndjson export for each test in tests/."

  FLAGS:
    v, verbose;  "Print each command as it runs, with its timing and memory."
    force;       "Rebuild even when the export already matches its inputs."
    "skip-ci";   "Skip tests marked skip_on_ci."

  ARGS:
    ...names : String; "Test names or globs; all tests when omitted."
]

def buildCheckerCmd := `[Cli|
  "build-checker" VIA runBuildChecker;
  "Check out and build the current revision of each checker in checkers/."

  FLAGS:
    v, verbose;                 "Print each command as it runs, with its timing and memory."
    checker : Array String;     "Checker names or globs; all checkers when omitted."
    pin : String;               "Build this pin instead of the configured one."
]

def runCmd := `[Cli|
  run VIA runRun;
  "Run the checker revisions that have no result yet for a test, and record them."

  FLAGS:
    v, verbose;              "Print each command as it runs, with its timing and memory."
    force;                   "Rerun pairs that already have a result."
    checker : Array String;  "Checker names or globs; all checkers when omitted."
    pin : String;            "Run this pin instead of the configured one."
    test : Array String;     "Test names or globs; all built tests when omitted."
]

def siteDataCmd := `[Cli|
  "site-data" VIA runSiteData;
  "Regenerate site-data/arena.json from _build/ and _results/."

  FLAGS:
    v, verbose; "Print each command as it runs, with its timing and memory."
]

def buildSiteCmd := `[Cli|
  "build-site" VIA runBuildSite;
  "Regenerate the site data, render the Verso site and pack the test tarball."

  FLAGS:
    v, verbose;        "Print each command as it runs, with its timing and memory."
    outdir : String;   "Directory to write the site into."

  EXTENSIONS:
    defaultValues! #[("outdir", "_out")]
]

def lkaCmd : Cmd := `[Cli|
  lka NOOP;
  "Build, run and publish the Lean Kernel Arena."

  SUBCOMMANDS:
    buildTestCmd;
    buildCheckerCmd;
    runCmd;
    siteDataCmd;
    buildSiteCmd
]

def main (argv : List String) : IO UInt32 := do
  if argv.isEmpty then
    lkaCmd.printHelp
    return 0
  try
    lkaCmd.validate argv
  catch error =>
    IO.eprintln s!"lka: {error}"
    return 1
