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
  forEachReporting configs fun config => buildTest config revision

def runBuildChecker (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  let configs ←
    select CheckerConfig.name "checkers" (p.variableArgsAs! String) (← loadCheckerConfigs)
  forEachReporting configs buildChecker

def runRun (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  let configs ←
    select CheckerConfig.name "checkers" (globsOf p "checker") (← loadCheckerConfigs)
  let mut built := #[]
  let mut unbuilt := #[]
  for config in configs do
    if ← config.isBuilt then built := built.push config else unbuilt := unbuilt.push config
  unless unbuilt.isEmpty do
    IO.println s!"Skipping {unbuilt.size} checker(s) that weren't built: \
{", ".intercalate (unbuilt.map (·.name)).toList}"
  let tests ← select TestStats.name "tests" (globsOf p "test") (← loadTestStats)
  if built.isEmpty then
    IO.println "No built checkers found."
    return 0
  if tests.isEmpty then
    IO.println "No built tests found."
    return 0
  reportTally tests (← runCheckers built tests)
  return 0

def runSiteData (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  generateSiteData
  return 0

def renderTutorial (outdir : System.FilePath) : IO Unit := do
  let tutorialTests := builtTestsDir / "tutorial"
  unless ← tutorialTests.isDir do
    IO.println "  No tutorial tests built; skipping the tutorial viewer"
    return
  let build ← runShell "lake build" (cwd := "test-printer") (printOnFailure := true)
  unless build.ok do throw <| .userError "failed to build test-printer"
  let target := outdir / "tutorial" / "index.html"
  IO.FS.createDirAll (outdir / "tutorial")
  let printer ← run {
    cmd := "lake"
    args := #["exe", "test-printer", (← IO.FS.realPath tutorialTests).toString,
      (← absolute target).toString]
    cwd := "test-printer"
  }
  unless printer.ok do throw <| .userError s!"test-printer failed: {printer.stderr}"
  IO.println s!"Generated: {target}"

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
  renderTutorial outdir
  IO.println s!"\nSite built successfully in: {outdir}"
  return 0

def buildTestCmd := `[Cli|
  "build-test" VIA runBuildTest;
  "Build the .ndjson export for each test in tests/."

  FLAGS:
    v, verbose;  "Print each command as it runs, with its timing and memory."
    "skip-ci";   "Skip tests marked skip_on_ci."

  ARGS:
    ...names : String; "Test names or globs; all tests when omitted."
]

def buildCheckerCmd := `[Cli|
  "build-checker" VIA runBuildChecker;
  "Check out and build each checker in checkers/."

  FLAGS:
    v, verbose; "Print each command as it runs, with its timing and memory."

  ARGS:
    ...names : String; "Checker names or globs; all checkers when omitted."
]

def runCmd := `[Cli|
  run VIA runRun;
  "Run checkers over built tests and record the results in _results/."

  FLAGS:
    v, verbose;              "Print each command as it runs, with its timing and memory."
    checker : Array String;  "Checker names or globs; all built checkers when omitted."
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
  "Regenerate the site data, render the Verso site, the tutorial viewer and the test tarball."

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
