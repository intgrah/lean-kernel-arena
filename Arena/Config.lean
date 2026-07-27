import Lake.Toml
import Arena.Util

open Lake (DecodeToml)
open Lake.Toml (Table Value EDecodeM throwDecodeErrorAt)

namespace Arena

inductive Expectation
  | accept
  | reject
deriving DecidableEq, Repr

def Expectation.toString : Expectation → String
  | .accept => "accept"
  | .reject => "reject"

instance : ToString Expectation := ⟨Expectation.toString⟩

def Expectation.ofString? : String → Option Expectation
  | "accept" => some .accept
  | "reject" => some .reject
  | _ => none

inductive Source
  | git (url : String) (ref rev : Option String)
  | localDir (path : String)
  | leanFile (path : String)
  | empty
deriving Repr

inductive Production
  | exportModule (module : String)
  | script (command : String) (multiple : Bool)
  | staticFile (path : String)
deriving Repr

structure TestConfig where
  name : String
  source : Source
  production : Production
  preBuild : Option String
  exportDecls : Array String
  expectation : Option Expectation
  comparePerf : Bool
  skipOnCi : Bool
deriving Repr

structure CheckerConfig where
  name : String
  version : Option String
  source : Source
  buildCommand : Option String
  runCommand : String
  disabled : Bool
  serious : Bool
  threads : Nat
deriving Repr

def CheckerConfig.displayVersion (c : CheckerConfig) : String :=
  match c.version with
  | some v => v
  | none => match c.source with
    | .git _ (some ref) (some rev) => s!"{ref} ({rev.take 7})"
    | _ => "unknown"

namespace Decode

private def decodeExpectation (v : Value) : EDecodeM Expectation := do
  let s ← v.decodeString
  match Expectation.ofString? s with
  | some e => return e
  | none => throwDecodeErrorAt v.ref s!"expected \"accept\" or \"reject\", got \"{s}\""

instance : DecodeToml Expectation := ⟨decodeExpectation⟩

private def flag (t : Table) (key : Lean.Name) : EDecodeM Bool :=
  return (← t.decode? (α := Bool) key).getD false

private def decodeSource (t : Table) : EDecodeM Source := do
  let url? ← t.decode? (α := String) `url
  let dir? ← t.decode? (α := String) `dir
  let leanfile? ← t.decode? (α := String) `leanfile
  let ref? ← t.decode? (α := String) `ref
  let rev? ← t.decode? (α := String) `rev
  match url?, dir?, leanfile? with
  | some url, none, none => return .git url ref? rev?
  | none, some dir, none => return .localDir dir
  | none, none, some leanfile => return .leanFile leanfile
  | none, none, none => return .empty
  | _, _, _ =>
    throwDecodeErrorAt .missing "at most one of `url`, `dir`, `leanfile` may be given"

private def decodeProduction (t : Table) (source : Source) : EDecodeM Production := do
  let module? ← t.decode? (α := String) `module
  let run? ← t.decode? (α := String) `run
  let file? ← t.decode? (α := String) `file
  let multiple ← flag t `multiple
  match module?, run?, file?, source with
  | some module, none, none, _ => return .exportModule module
  | none, some run, none, _ => return .script run multiple
  | none, none, some file, .empty => return .staticFile file
  | none, none, none, .leanFile _ => return .exportModule "Test"
  | none, none, some _, _ =>
    throwDecodeErrorAt .missing "`file` cannot be combined with a source"
  | none, none, none, _ =>
    throwDecodeErrorAt .missing "one of `module`, `run`, `file`, `leanfile` is required"
  | _, _, _, _ =>
    throwDecodeErrorAt .missing "at most one of `module`, `run`, `file` may be given"

private def decodeTest (name : String) (t : Table) : EDecodeM TestConfig := do
  let source ← decodeSource t
  let production ← decodeProduction t source
  let exportDecls := (← t.decode? (α := Array String) `export_decls).getD #[]
  if !exportDecls.isEmpty then
    if let .script .. := production then
      throwDecodeErrorAt .missing "`export_decls` requires `module` or `leanfile`"
    if let .staticFile _ := production then
      throwDecodeErrorAt .missing "`export_decls` requires `module` or `leanfile`"
  if let .script _ true := production then
    if (← t.decode? (α := Expectation) `outcome).isSome then
      throwDecodeErrorAt .missing "`multiple` tests take their outcome from good/ and bad/"
  return {
    name, source, production, exportDecls
    preBuild := ← t.decode? (α := String) `pre_build
    expectation := ← t.decode? (α := Expectation) `outcome
    comparePerf := ← flag t `compare_perf
    skipOnCi := ← flag t `skip_on_ci
  }

private def decodeChecker (name : String) (t : Table) : EDecodeM CheckerConfig := do
  let source ← decodeSource t
  if let .leanFile _ := source then
    throwDecodeErrorAt .missing "checkers have no `leanfile` source"
  return {
    name, source
    version := ← t.decode? (α := String) `version
    buildCommand := ← t.decode? (α := String) `build
    runCommand := ← t.decode (α := String) `run
    disabled := ← flag t `disable
    serious := (← t.decode? (α := Bool) `serious).getD true
    threads := (← t.decode? (α := Nat) `threads).getD 1
  }

private def loadTable (path : System.FilePath) : IO Table := do
  let text ← IO.FS.readFile path
  let ictx := Lean.Parser.mkInputContext text path.toString
  match ← (Lake.Toml.loadToml ictx).toBaseIO with
  | .ok table => return table
  | .error log =>
    let messages ← log.toList.mapM (·.toString)
    throw <| .userError s!"{path}:\n{"".intercalate messages}"

private def decodeFile (α) (path : System.FilePath) (name : String)
    (decoder : String → Table → EDecodeM α) : IO α := do
  match decoder name (← loadTable path) #[] with
  | .ok config _ => return config
  | .error _ errors =>
    let lines := errors.toList.map (s!"  {·.msg}")
    throw <| .userError s!"{path}:\n{"\n".intercalate lines}"

end Decode

def testsDir : System.FilePath := "tests"
def checkersDir : System.FilePath := "checkers"

def loadTestConfig (name : String) : IO TestConfig :=
  Decode.decodeFile _ (testsDir / (name ++ ".toml")) name Decode.decodeTest

def loadCheckerConfig (name : String) : IO CheckerConfig :=
  Decode.decodeFile _ (checkersDir / (name ++ ".toml")) name Decode.decodeChecker

def loadTestConfigs : IO (Array TestConfig) := do
  (← findNamesUnder testsDir ".toml").mapM loadTestConfig

def loadCheckerConfigs : IO (Array CheckerConfig) := do
  let all ← (← findNamesIn checkersDir ".toml").mapM loadCheckerConfig
  return all.filter (!·.disabled)

def selectByPatterns (patterns : Array String) (names : Array String) : Array String :=
  if patterns.isEmpty then names
  else names.filter fun name => patterns.any (globMatch · name)

end Arena
