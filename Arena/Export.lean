import Arena.Export.Env
import Arena.Export.Print

namespace Arena.Export

open Lean SubVerso.Highlighting

def readExport (path : System.FilePath) : IO ExportedEnv := do
  IO.FS.withFile path .read fun handle =>
    parseStream (IO.FS.Stream.ofHandle handle)

private def declarationItem (name : Name) (code : Highlighted) : SubVerso.Module.ModuleItem :=
  { range := none, kind := `Arena.Export, defines := #[name], code }

/--
Whether the export merely repeats a declaration Lean already has. Repeating one is how a test
brings in the context it needs; changing one is how a test smuggles in an unsound definition, so
only an identical repeat is uninteresting.
-/
private def repeatsCore (env : Environment) (ci : ConstantInfo) : Bool :=
  match env.find? ci.name with
  | some core => core.type == ci.type && core.value? == ci.value?
  | none => false

/--
Renders the declarations an export adds to Lean's own `Init`, in the order the exporter emitted
them. The constants are inserted without checking, since the tests under measurement are
deliberately malformed.
-/
def coreEnvironment : IO Environment := do
  initSearchPath (← findSysroot)
  importModules #[{ module := `Init }] {}

def renderExport (env : Environment) (path : System.FilePath) : IO SubVerso.Module.Module := do
  let exported ← readExport path
  let context : Core.Context := { fileName := path.toString, fileMap := default }
  let (items, _) ← (do
    let core ← getEnv
    let constants := exported.constOrder.filterMap (exported.constMap[·]?)
    addConstInfos constants
    let mut items := #[]
    for ci in constants do
      unless repeatsCore core ci do
        items := items.push (declarationItem ci.name (← Meta.MetaM.run' (ppDeclaration ci)))
    return items : CoreM _).toIO context { env }
  return ⟨items⟩

end Arena.Export
