import Cli
import Arena.SiteData

open Arena Cli

def applyVerbosity (p : Parsed) : IO Unit :=
  verboseRef.set (p.hasFlag "verbose")

def selectorsOf (p : Parsed) (flag : String) : Array String :=
  match p.flag? flag with
  | some value => value.as! (Array String)
  | none => #[]

def select (name : α → String) (kind : String) (selectors : Array String)
    (items : Array α) : IO (Array α) := do
  let chosen := selectByPatterns name selectors items
  if chosen.isEmpty && !selectors.isEmpty then
    throw <| .userError s!"no {kind} match {" ".intercalate selectors.toList}"
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
    if p.hasFlag "rebuild" then pure configs else configs.filterM (fun c => return !(← isCurrent c))
  let current := configs.size - stale.size
  if current > 0 then
    IO.println s!"{current} test(s) already match their inputs; rebuilding {stale.size}"
  forEachReporting stale fun config => buildTest config revision

def runPrepare (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  prepareCorpus
  return 0

def runSiteData (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  generateSiteData
  return 0

def runBuildSite (p : Parsed) : IO UInt32 := do
  applyVerbosity p
  let outdir : System.FilePath := p.flag! "outdir" |>.as! String
  prepareCorpus
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
    rebuild;     "Rebuild even when the export already matches its inputs."
    "skip-ci";   "Skip tests marked skip_on_ci."

  ARGS:
    ...names : String; "Test names, or a directory as `Perf/`; all tests when omitted."
]

def prepareCmd := `[Cli|
  prepare VIA runPrepare;
  "Pack the test tarball and render the sources and exports the site needs."

  FLAGS:
    v, verbose; "Print each command as it runs, with its timing and memory."
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
    prepareCmd;
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
