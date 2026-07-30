import Lake.Toml
import Arena.Util

open Lake (DecodeToml)
open Lake.Toml (Table Value EDecodeM throwDecodeErrorAt)

namespace Arena

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

namespace Decode

private def decodeExpectation (v : Value) : EDecodeM Expectation := do
  let s ← v.decodeString
  match Expectation.ofString? s with
  | some e => return e
  | none => throwDecodeErrorAt v.ref s!"expected \"accept\", \"reject\" or \"either\", got \"{s}\""

instance : DecodeToml Expectation := ⟨decodeExpectation⟩

private def flag (t : Table) (key : Lean.Name) : EDecodeM Bool :=
  return (← t.decode? (α := Bool) key).getD false

private def decodeSource (t : Table) : EDecodeM Source := do
  let url? ← t.decode? (α := String) `url
  let dir? ← t.decode? (α := String) `dir
  let leanfile? ← t.decode? (α := String) `leanFile
  let ref? ← t.decode? (α := String) `ref
  let rev? ← t.decode? (α := String) `rev
  match url?, dir?, leanfile? with
  | some url, none, none => return .git url ref? rev?
  | none, some dir, none => return .localDir dir
  | none, none, some leanfile => return .leanFile leanfile
  | none, none, none => return .empty
  | _, _, _ =>
    throwDecodeErrorAt .missing "at most one of `url`, `dir`, `leanFile` may be given"

def moduleOfLeanFile (path : String) : String :=
  let rel := if path.startsWith "Tests/" then (path.drop 6).toString else path
  (dropSuffix ".lean" rel).replace "/" "."

private def decodeProduction (t : Table) (source : Source) : EDecodeM Production := do
  let module? ← t.decode? (α := String) `module
  let run? ← t.decode? (α := String) `run
  let file? ← t.decode? (α := String) `file
  let multiple ← flag t `multiple
  match module?, run?, file?, source with
  | some module, none, none, _ => return .exportModule module
  | none, some run, none, _ => return .script run multiple
  | none, none, some file, .empty => return .staticFile file
  | none, none, none, .leanFile path => return .exportModule (moduleOfLeanFile path)
  | none, none, some _, _ =>
    throwDecodeErrorAt .missing "`file` cannot be combined with a source"
  | none, none, none, _ =>
    throwDecodeErrorAt .missing "one of `module`, `run`, `file`, `leanFile` is required"
  | _, _, _, _ =>
    throwDecodeErrorAt .missing "at most one of `module`, `run`, `file` may be given"

private def decodeTest (name : String) (t : Table) : EDecodeM TestConfig := do
  let source ← decodeSource t
  let production ← decodeProduction t source
  let exportDecls := (← t.decode? (α := Array String) `exportDecls).getD #[]
  if !exportDecls.isEmpty then
    if let .script .. := production then
      throwDecodeErrorAt .missing "`exportDecls` requires `module` or `leanFile`"
    if let .staticFile _ := production then
      throwDecodeErrorAt .missing "`exportDecls` requires `module` or `leanFile`"
  if let .script _ true := production then
    if (← t.decode? (α := Expectation) `outcome).isSome then
      throwDecodeErrorAt .missing "`multiple` tests take their outcome from good/ and bad/"
  return {
    name, source, production, exportDecls
    preBuild := ← t.decode? (α := String) `preBuild
    expectation := ← t.decode? (α := Expectation) `outcome
    comparePerf := ← flag t `comparePerf
    skipOnCi := ← flag t `skipOnCi
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

def loadTestConfig (name : String) : IO TestConfig :=
  Decode.decodeFile _ (testsDir / (name ++ ".toml")) name Decode.decodeTest

def isLakeBookkeeping (name : String) : Bool :=
  name == "lakefile" || name.endsWith "/lakefile"

def loadTestConfigs : IO (Array TestConfig) := do
  let names := (← findNamesUnder testsDir ".toml" 2).filter (!isLakeBookkeeping ·)
  names.mapM loadTestConfig

def selectByPatterns (name : α → String) (selectors : Array String) (items : Array α) :
    Array α :=
  if selectors.isEmpty then items
  else items.filter fun item => selectors.any (selects · (name item))

end Arena
