import Lean
import Export.Parse

open Lean

structure ReplayState where
  env : Kernel.Environment
  visited : NameSet := ∅

abbrev ReplayM := ReaderT (Std.HashMap Name ConstantInfo) (StateRefT ReplayState IO)

private def throwKernel (ex : Kernel.Exception) : ReplayM α := do
  throw (.userError (← (ex.toMessageData {}).toString))

private def addChecked (d : Declaration) : ReplayM Unit := do
  match (← get).env.addDeclCore 0 d none with
  | .ok env => modify fun s => { s with env := env }
  | .error ex => throwKernel ex

private def addUnchecked (name : Name) (d : Declaration) : ReplayM Unit := do
  if ((← get).env.find? name).isSome then
    throw (.userError s!"'{name}' has already been declared")
  match (← get).env.addDeclWithoutChecking d with
  | .ok env => modify fun s => { s with env := env }
  | .error ex => throwKernel ex

private def markVisited (names : List Name) : ReplayM Unit :=
  modify fun s => { s with visited := names.foldl NameSet.insert s.visited }

private partial def replay (name : Name) : ReplayM Unit := do
  if (← get).visited.contains name then return
  markVisited [name]
  let consts ← read
  let some ci := consts[name]? | return
  match ci with
  | .defnInfo info =>
    for n in ci.getUsedConstantsAsSet do replay n
    addUnchecked name (.defnDecl info)
  | .thmInfo info =>
    for n in ci.getUsedConstantsAsSet do replay n
    addUnchecked name (.thmDecl info)
  | .axiomInfo info =>
    for n in ci.getUsedConstantsAsSet do replay n
    addUnchecked name (.axiomDecl info)
  | .opaqueInfo info =>
    for n in ci.getUsedConstantsAsSet do replay n
    addUnchecked name (.opaqueDecl info)
  | .inductInfo info =>
    let ivs : List InductiveVal := info.all.filterMap fun n =>
      match (consts[n]? : Option ConstantInfo) with
      | some (.inductInfo iv) => some iv
      | _ => none
    markVisited info.all
    let types ← ivs.mapM fun iv => do
      for n in iv.type.getUsedConstants do replay n
      let ctors ← iv.ctors.mapM fun cn => do
        let some (.ctorInfo cv) := (consts[cn]? : Option ConstantInfo)
          | throw (.userError s!"missing constructor {cn} of {iv.name}")
        for n in cv.type.getUsedConstants do replay n
        return ({ name := cv.name, type := cv.type } : Constructor)
      return ({ name := iv.name, type := iv.type, ctors } : InductiveType)
    addChecked (.inductDecl info.levelParams info.numParams types false)
  | .ctorInfo info => replay info.induct
  | .recInfo info => for n in info.all do replay n
  | .quotInfo _ =>
    replay `Eq
    markVisited [`Quot, `Quot.mk, `Quot.lift, `Quot.ind]
    addChecked Declaration.quotDecl

def build (consts : Std.HashMap Name ConstantInfo) : IO Kernel.Environment := do
  let base ← mkEmptyEnvironment
  let safe := consts.filter fun _ ci => !ci.isUnsafe && !ci.isPartial
  let replayAll : ReplayM Unit := safe.toList.forM fun (n, _) => replay n
  let (_, s) ← (replayAll.run safe).run { env := base.toKernelEnv }
  return s.env

inductive Outcome
  | accept (checked : Nat)
  | reject (name : Name)
  | decline (name : Name)

def elabCheck (ci : ConstantInfo) : MetaM Unit := do
  unless ci.levelParams.eraseDups.length == ci.levelParams.length do
    throwError "duplicate universe parameter in {ci.name}"
  Meta.check ci.type
  let sort ← Meta.inferType ci.type
  unless (← Meta.withTransparency .all <| Meta.whnf sort).isSort do
    throwError "type of {ci.name} is not a sort"
  if ci matches .thmInfo _ then
    unless ← Meta.isProp ci.type do
      throwError "type of theorem {ci.name} is not a proposition"
  let some value := ci.value? (allowOpaque := true) | return
  Meta.check value
  let valType ← Meta.inferType value
  unless (← Meta.withTransparency .all <| Meta.isDefEq valType ci.type) do
    throwError "type mismatch for {ci.name}"

def isElabChecked (ci : ConstantInfo) : Bool :=
  ci matches .defnInfo _ | .thmInfo _ | .axiomInfo _ | .opaqueInfo _

def checkAll (env : Environment) (ctx : Core.Context) (targets : Array ConstantInfo) :
    BaseIO Outcome := do
  for ci in targets do
    match ← (Core.CoreM.run' (elabCheck ci).run' ctx { env }).toBaseIO with
    | .ok _ => pure ()
    | .error ex => return if ex.isRuntime then .decline ci.name else .reject ci.name
  return .accept targets.size

def dumpModule : Name := `MetaCheckDump

def runMetaCheck (exported : Export.ExportedEnv) : IO UInt32 := do
  let consts := exported.constMap
  let kenv ← build consts
  let env := (Environment.ofKernelEnv kenv).setMainModule dumpModule
  IO.FS.withTempDir fun tmp => do
    writeModule env (tmp / s!"{dumpModule}.olean")
    searchPathRef.set [tmp]
    let importedEnv ← importModules #[{ module := dumpModule, importAll := true }] {}
    let elabTargets := consts.toList.filterMap (fun (_, ci) =>
      if !ci.isUnsafe && !ci.isPartial && isElabChecked ci then some ci else none)
      |>.toArray
    let ctx : Core.Context := {
      fileName := "<metacheck>"
      fileMap := default
      maxHeartbeats := 0
      maxRecDepth := 10000
      options := Lean.maxRecDepth.set {} 10000
    }
    match ← checkAll importedEnv ctx elabTargets with
    | .accept count =>
      IO.println s!"Accepted {count} declarations."
      return 0
    | .reject n =>
      IO.eprintln s!"rejected at {n}"
      return 1
    | .decline n =>
      IO.eprintln s!"declined at {n}"
      return 2

def main : List String → IO UInt32
  | [inputPath] => do
    let handle ← IO.FS.Handle.mk inputPath .read
    let exported ← Export.parseStream (.ofHandle handle)
    runMetaCheck exported
  | _ => do
    IO.eprintln "usage: metacheck <export.ndjson>"
    return 1
