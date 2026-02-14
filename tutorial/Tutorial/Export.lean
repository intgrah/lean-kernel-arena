import Export
open Lean

initialize importedRecursorMap : IO.Ref (NameMap NameSet) ← do
  IO.mkRef {}

def addRecInfo (constInfo : ConstantInfo) (recursorMap : NameMap NameSet) : NameMap NameSet :=
  if let .recInfo recVal := constInfo then
    recVal.all.foldl (init := recursorMap) fun recursorMap indName =>
      recursorMap.alter indName <|
        fun
        | none => some <| NameSet.empty.insert recVal.name
        | some recNames => some <| recNames.insert recVal.name
  else
    recursorMap

/--
Like initstate, but assumes that the imported entries in `env` are
always the same and cache them
-/
def initStateCached (env : Environment) (cliOptions : List String := []) : M Unit := do
  let mut recursorMap : NameMap NameSet := (← importedRecursorMap.get)
  if recursorMap.isEmpty then
    for (_, constInfo) in env.constants.map₁ do
      recursorMap := addRecInfo constInfo recursorMap
    importedRecursorMap.set recursorMap
  for (_, constInfo) in env.constants.map₂ do
    recursorMap := addRecInfo constInfo recursorMap
  modify fun st => { st with
    exportMData  := cliOptions.any  (· == "--export-mdata")
    exportUnsafe := cliOptions.any (· == "--export-unsafe")
    recursorMap
  }

def exportDeclsFromEnv (env : Lean.Environment) (constants : Array Name) : IO Unit := do
  initSearchPath (← findSysroot)
  M.run env do
    initStateCached env
    dumpMetadata
    for c in constants do
      modify (fun st => { st with noMDataExprs := {} })
      let _ ← dumpConstant c
