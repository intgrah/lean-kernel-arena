import Arena.Repo

namespace Arena

def lean4exportUrl : String := "https://github.com/leanprover/lean4export"

private def freshDir (path : System.FilePath) : IO Unit := do
  removeIfExists path
  IO.FS.createDirAll path

private def standaloneLakefile : String :=
  "name = \"test\"\n\n[[lean_lib]]\nname = \"Test\"\n"

def setupSource (source : Source) (work : System.FilePath) (dirBase : System.FilePath) :
    IO System.FilePath := do
  freshDir work
  let src := work / "src"
  match source with
  | .git url ref rev =>
    IO.println s!"  Cloning {url}..."
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
    IO.println s!"  Copied {source} to {src}"
  | .leanFile path =>
    unless ← System.FilePath.pathExists path do
      throw <| .userError s!"source file not found: {path}"
    IO.FS.createDirAll src
    copyFile path (src / "Test.lean")
    copyFile (testsDir / "lean-toolchain") (src / "lean-toolchain")
    IO.FS.writeFile (src / "lakefile.toml") standaloneLakefile
    IO.println s!"  Prepared standalone Test module from {path}"
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
  IO.println s!"  Cloning lean4export for toolchain {toolchain}..."
  let staging := lean4exportRoot / (toolchainSlug toolchain ++ ".tmp")
  removeIfExists staging
  IO.FS.createDirAll staging
  let clone ← run {
    cmd := "git"
    args := #["clone", "--branch", "master", lean4exportUrl, staging.toString]
  }
  unless clone.ok do throw <| .userError s!"failed to clone lean4export: {clone.stderr}"
  IO.FS.writeFile (staging / "lean-toolchain") (toolchain ++ "\n")
  IO.println s!"  Building lean4export with toolchain {toolchain}..."
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

private def gatherStats (name : String) (config : TestConfig) (ndjson : System.FilePath)
    (expectation : Option Expectation) (generatedDescription : Option String)
    (links : SourceLinks) : IO TestStats := do
  let (size, lines) ← measureNdjson ndjson
  return {
    toExportMeta := ← readExportMeta ndjson
    name, size, lines, expectation, generatedDescription
    comparePerf := config.comparePerf
    declarationUrl := links.declarationUrl
    sourceUrl := links.sourceUrl
  }

private def runPreBuild (config : TestConfig) (cwd : System.FilePath) : IO Unit := do
  let some command := config.preBuild | return
  IO.println s!"  Running pre-build: {command}"
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
  IO.println s!"  Running: {command}"
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
  IO.println s!"  Created {stats.ndjsonPath} ({formatMemory stats.size.toFloat}, \
{formatUnitless stats.lines.toFloat} lines)"

def buildTest (config : TestConfig) (revision : Option String) : IO Unit := do
  let links := config.sourceLinks revision
  IO.FS.createDirAll builtTestsDir
  if let .staticFile path := config.production then
    IO.println s!"Creating test: {config.name} (static file)"
    return ← buildSingle config (fun staging => copyFile path staging) links
  let src ← setupSource config.source (workRoot / "tests" / config.name) "."
  match config.production with
  | .staticFile _ => pure ()
  | .exportModule module =>
    IO.println s!"Creating test: {config.name} (export of {module})"
    let lean4export ← setupLean4Export (← readToolchain src)
    runPreBuild config src
    IO.println s!"  Building module {module}..."
    let build ← runShell s!"lake build {module}" (cwd := src) (printOnFailure := true)
    unless build.ok do throw <| .userError s!"build of {module} failed"
    let declList :=
      if config.exportDecls.isEmpty then ""
      else s!" ({", ".intercalate config.exportDecls.toList})"
    IO.println s!"  Exporting module {module}{declList}..."
    buildSingle config
      (fun staging => runLean4Export lean4export module config.exportDecls src staging) links
  | .script command multiple =>
    IO.println s!"Creating test: {config.name} (script{if multiple then ", multiple" else ""})"
    runPreBuild config src
    if multiple then
      let count ← buildMultiple config command src links
      IO.println s!"  Created {count} subtests in {builtTestsDir / config.name}"
    else
      buildSingle config (fun staging => do
        IO.println s!"  Running: {command}"
        let outcome ← runShell command (cwd := src)
          (env := #[("OUT", staging.toString)]) (printOnFailure := true)
        unless outcome.ok do throw <| .userError "test script failed") links

def checkerWorkDir (config : CheckerConfig) : System.FilePath :=
  match config.source with
  | .empty => builtCheckersDir / config.name
  | _ => builtCheckersDir / config.name / "src"

def CheckerConfig.isBuilt (config : CheckerConfig) : IO Bool :=
  (builtCheckersDir / config.name).pathExists

def buildChecker (config : CheckerConfig) : IO Unit := do
  IO.println s!"Building checker: {config.name} (version: {config.displayVersion})"
  discard <| setupSource config.source (builtCheckersDir / config.name) checkersDir
  let work := checkerWorkDir config
  if let some command := config.buildCommand then
    IO.println s!"  Building: {command}"
    let outcome ← runShell command (cwd := work) (printOnFailure := true)
    unless outcome.ok do throw <| .userError "build failed"
  IO.println s!"  Checker {config.name} built successfully"

end Arena
