import Export
open Lean

def exportDeclsFromEnv (env : Lean.Environment) (constants : Array Name) : IO Unit := do
  initSearchPath (← findSysroot)
  M.run env do
    modify (fun st => { st with
      exportMData  := false
      exportUnsafe := false
    })
    dumpMetadata
    for c in constants do
      modify (fun st => { st with noMDataExprs := {} })
      let _ ← dumpConstant c
