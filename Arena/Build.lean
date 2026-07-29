import Arena.Repo

namespace Arena

def lean4exportUrl : String := "https://github.com/leanprover/lean4export"

private def freshDir (path : System.FilePath) : IO Unit := do
  removeIfExists path
  IO.FS.createDirAll path

def setupSource (source : Source) (work : System.FilePath) (dirBase : System.FilePath) :
    IO System.FilePath := do
  freshDir work
  let src := work / "src"
  match source with
  | .git url ref rev =>
    note 1 s!"Cloning {url}..."
    let branch := match ref with | some ref => #["--branch", ref] | none => #[]
    let clone ← run { cmd := "git", args := #["clone"] ++ branch ++ #[url, src.toString] }
    unless clone.ok do throw <| .userError s!"failed to clone {url}: {clone.stderr}"
    if let some rev := rev then
      let checkout ← run { cmd := "git", args := #["checkout", rev], cwd := src }
      unless checkout.ok do throw <| .userError s!"failed to check out {rev}: {checkout.stderr}"
  | .localDir path =>
    let source := dirBase / path
    unless ← source.pathExists do throw <| .userError s!"source directory not found: {source}"
    copyTree source src
    note 1 s!"Copied {source} to {src}"
  | .leanFile path =>
    throw <| .userError s!"leanFile sources build in place, not via setupSource: {path}"
  | .empty =>
    IO.FS.createDirAll src
  return src

private def readToolchain (dir : System.FilePath) : IO String := do
  let path := dir / "lean-toolchain"
  unless ← path.pathExists do throw <| .userError s!"no lean-toolchain in {dir}"
  return (← IO.FS.readFile path).trimAscii.toString

private def toolchainSlug (toolchain : String) : String :=
  toolchain.map fun c => if c == '/' || c == ':' then '_' else c

def setupLean4Export (toolchain : String) : IO System.FilePath := do
  let dir ← absolute (lean4exportRoot / toolchainSlug toolchain)
  if ← dir.pathExists then return dir
  note 1 s!"Cloning lean4export for toolchain {toolchain}..."
  let staging := lean4exportRoot / (toolchainSlug toolchain ++ ".tmp")
  removeIfExists staging
  IO.FS.createDirAll staging
  let clone ← run {
    cmd := "git"
    args := #["clone", "--branch", "master", lean4exportUrl, staging.toString]
  }
  unless clone.ok do throw <| .userError s!"failed to clone lean4export: {clone.stderr}"
  IO.FS.writeFile (staging / "lean-toolchain") (toolchain ++ "\n")
  note 1 s!"Building lean4export with toolchain {toolchain}..."
  let build ← runShell "lake build" (cwd := staging) (printOnFailure := true)
  unless build.ok do throw <| .userError "failed to build lean4export"
  IO.FS.rename staging dir
  return dir

private def shellQuote (s : String) : String :=
  "'" ++ s.replace "'" "'\\''" ++ "'"

def runLean4Export (lean4export : System.FilePath) (module : String) (decls : Array String)
    (cwd outFile : System.FilePath) : IO Unit := do
  let binary := lean4export / ".lake" / "build" / "bin" / "lean4export"
  unless ← binary.pathExists do
    throw <| .userError s!"lean4export binary not found at {binary}"
  let declArgs :=
    if decls.isEmpty then "" else " -- " ++ " ".intercalate (decls.map shellQuote).toList
  let script := s!"lake env {binary} {module}{declArgs} > {shellQuote outFile.toString}"
  let exported ← runShell script (cwd := cwd) (printOnFailure := true)
  unless exported.ok do throw <| .userError s!"export of {module} failed"

private def newlineCount (bytes : ByteArray) : Nat :=
  bytes.foldl (init := 0) fun count byte => if byte == 10 then count + 1 else count

partial def measureNdjson (path : System.FilePath) : IO (Nat × Nat) := do
  let handle ← IO.FS.Handle.mk path .read
  let size := (← path.metadata).byteSize.toNat
  return (size, ← go handle 0)
where
  go (handle : IO.FS.Handle) (lines : Nat) : IO Nat := do
    let chunk ← handle.read 1048576
    if chunk.isEmpty then return lines else go handle (lines + newlineCount chunk)

def readExportMeta (path : System.FilePath) : IO ExportMeta := do
  let handle ← IO.FS.Handle.mk path .read
  let firstLine ← handle.getLine
  let .ok json := Lean.Json.parse firstLine | return {}
  let .ok header := json.getObjVal? "meta" | return {}
  let lean := (header.getObjVal? "lean").toOption.getD Lean.Json.null
  let exporter := (header.getObjVal? "exporter").toOption.getD Lean.Json.null
  return {
    exporterVersion := (exporter.getObjValAs? String "version").toOption
    leanVersion := (lean.getObjValAs? String "version").toOption
  }

/--
Everything that decides what an export will contain: the test's own configuration, the source it
is produced from, and the toolchain it is produced with.
-/
def testRecipe (config : TestConfig) : IO String := do
  let localFile ← match config.source, config.production with
    | .leanFile path, _ | _, .staticFile path =>
      if ← System.FilePath.pathExists path then digestFile path else pure ""
    | _, _ => pure ""
  let toolchain ← readToolchain "."
  return digestString (String.intercalate " " [reprStr config, localFile, toolchain])

private def gatherStats (name : String) (config : TestConfig) (ndjson : System.FilePath)
    (expectation : Option Expectation) (generatedDescription : Option String)
    (links : SourceLinks) : IO TestStats := do
  let (size, lines) ← measureNdjson ndjson
  return {
    toExportMeta := ← readExportMeta ndjson
    name, size, lines, expectation, generatedDescription
    comparePerf := config.comparePerf
    hash := ← digestFile ndjson
    recipeHash := ← testRecipe config
    declarationUrl := links.declarationUrl
    sourceUrl := links.sourceUrl
    sourceModule := match config.source with
      | .leanFile path => some (Decode.moduleOfLeanFile path)
      | _ => none
  }

private def runPreBuild (config : TestConfig) (cwd : System.FilePath) : IO Unit := do
  let some command := config.preBuild | return
  note 1 s!"Running pre-build: {command}"
  let outcome ← runShell command (cwd := cwd) (printOnFailure := true)
  unless outcome.ok do throw <| .userError "pre-build failed"

private def collectSubtests (dir : System.FilePath) : IO (Array (String × Expectation)) := do
  let mut found := #[]
  for (subdir, expectation) in [("good", Expectation.accept), ("bad", Expectation.reject)] do
    for name in ← findNamesIn (dir / subdir) ".ndjson" do
      found := found.push (name, expectation)
  return found

private def readSubtestDescription (path : System.FilePath) : IO (Option String) := do
  unless ← path.pathExists do return none
  return ((← readJsonFile path).getObjValAs? String "description").toOption

private def buildMultiple (config : TestConfig) (command : String) (src : System.FilePath)
    (links : SourceLinks) : IO Nat := do
  let staging ← absolute (builtTestsDir / (config.name ++ ".tmp"))
  freshDir staging
  note 1 s!"Running: {command}"
  let outcome ← runShell command (cwd := src)
    (env := #[("OUT", staging.toString)]) (printOnFailure := true)
  unless outcome.ok do throw <| .userError "test script failed"
  let subtests ← collectSubtests staging
  if subtests.isEmpty then
    throw <| .userError s!"no .ndjson files in {staging}/good/ or {staging}/bad/"
  for (subtest, expectation) in subtests do
    let subdir := if expectation == .accept then "good" else "bad"
    let ndjson := staging / subdir / (subtest ++ ".ndjson")
    let description ← readSubtestDescription (staging / subdir / (subtest ++ ".info.json"))
    let stats ← gatherStats s!"{config.name}/{subtest}" config ndjson (some expectation)
      description links
    writeJsonFile (staging / subdir / (subtest ++ ".stats.json")) (Lean.toJson stats)
  let final := builtTestsDir / config.name
  removeIfExists final
  IO.FS.rename staging final
  return subtests.size

private def buildSingle (config : TestConfig) (produce : System.FilePath → IO Unit)
    (links : SourceLinks) : IO Unit := do
  let ndjson := builtTestsDir / (config.name ++ ".ndjson")
  let staging ← absolute (builtTestsDir / (config.name ++ ".tmp"))
  if let some parent := ndjson.parent then IO.FS.createDirAll parent
  removeIfExists staging
  produce staging
  IO.FS.rename staging ndjson
  let stats ← gatherStats config.name config ndjson config.expectation none links
  writeJsonFile (builtTestsDir / (config.name ++ ".stats.json")) (Lean.toJson stats)
  note 1 s!"Created {ndjson} ({formatMemory stats.size.toFloat}, \
{formatUnitless stats.lines.toFloat} lines)"

def isCurrent (config : TestConfig) : IO Bool := do
  let stats := builtTestsDir / (config.name ++ ".stats.json")
  unless ← stats.pathExists do return false
  match Lean.fromJson? (α := TestStats) (← readJsonFile stats) with
  | Except.error _ => return false
  | Except.ok stored => return stored.recipeHash == (← testRecipe config)

def buildTest (config : TestConfig) (revision : Option String) : IO Unit := do
  let links := config.sourceLinks revision
  IO.FS.createDirAll builtTestsDir
  if let .staticFile path := config.production then
    IO.println s!"Creating test: {config.name} (static file)"
    return ← buildSingle config (fun staging => copyFile path staging) links
  let src ←
    match config.source with
    | .leanFile _ | .empty => pure ("." : System.FilePath)
    | source => setupSource source (workRoot / "tests" / config.name) "."
  match config.production with
  | .staticFile _ => pure ()
  | .exportModule module =>
    IO.println s!"Creating test: {config.name} (export of {module})"
    let lean4export ← setupLean4Export (← readToolchain src)
    runPreBuild config src
    note 1 s!"Building module {module}..."
    let build ← runShell s!"lake build {module}" (cwd := src) (printOnFailure := true)
    unless build.ok do throw <| .userError s!"build of {module} failed"
    let declList :=
      if config.exportDecls.isEmpty then ""
      else s!" ({", ".intercalate config.exportDecls.toList})"
    note 1 s!"Exporting module {module}{declList}..."
    buildSingle config
      (fun staging => runLean4Export lean4export module config.exportDecls src staging) links
  | .script command multiple =>
    IO.println s!"Creating test: {config.name} (script{if multiple then ", multiple" else ""})"
    runPreBuild config src
    if multiple then
      let count ← buildMultiple config command src links
      note 1 s!"Created {count} subtests in {builtTestsDir / config.name}"
    else
      buildSingle config (fun staging => do
        note 1 s!"Running: {command}"
        let outcome ← runShell command (cwd := src)
          (env := #[("OUT", staging.toString)]) (printOnFailure := true)
        unless outcome.ok do throw <| .userError "test script failed") links

def checkerRoot (config : CheckerConfig) (pin : Pin) : System.FilePath :=
  builtCheckersDir / config.name / pin.id

def checkerWorkDir (config : CheckerConfig) (pin : Pin) : System.FilePath :=
  match config.source with
  | .empty => checkerRoot config pin
  | _ => checkerRoot config pin / "src"

def applyPin (work : System.FilePath) : Pin → IO Unit
  | .toolchain name => IO.FS.writeFile (work / "lean-toolchain") (name ++ "\n")
  | .commit _ | .fixed => pure ()

def isBuilt (config : CheckerConfig) (pin : Pin) : IO Bool :=
  (checkerRoot config pin).pathExists

def buildChecker (config : CheckerConfig) (pin : Pin) : IO Unit := do
  IO.println s!"Building checker: {config.name}@{pin.id}"
  discard <| setupSource (config.sourceWith pin) (checkerRoot config pin) checkersDir
  let work := checkerWorkDir config pin
  applyPin work pin
  if let some command := config.buildCommand then
    note 1 s!"Building: {command}"
    let outcome ← runShell command (cwd := work) (printOnFailure := true)
    unless outcome.ok do throw <| .userError "build failed"
  note 1 s!"Checker {config.name}@{pin.id} built successfully"

end Arena
