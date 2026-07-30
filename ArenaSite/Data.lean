import Lean.Data.Json
import SubVerso.Module
import VersoBlog
import Arena.SiteData

open Lean
open Lean.Elab Term

namespace ArenaSite

open Arena (Expectation Status Attempt)
export Arena (Expectation Status Attempt)

deriving instance Quote for Arena.Expectation
deriving instance Quote for Arena.Status
deriving instance Quote for Arena.Attempt

export Arena (Metrics CheckerStats CheckerInfo TestStats ResultInfo TarballData BuildInfo
  Payload)

deriving instance Quote for Arena.Metrics
deriving instance Quote for Arena.CheckerStats
deriving instance Quote for Arena.CheckerInfo
deriving instance Quote for Arena.ExportMeta
deriving instance Quote for Arena.TestStats
deriving instance Quote for Arena.ResultInfo
deriving instance Quote for Arena.TarballInfo
deriving instance Quote for Arena.TarballData
deriving instance Quote for Arena.BuildInfo
deriving instance Quote for Arena.Payload

abbrev TestInfo := Arena.TestStats

def expectedSchemaVersion : Nat := 1

initialize payloadCache : IO.Ref (Option Payload) ← IO.mkRef none

private def readPayload : TermElabM Payload := do
  let path := Arena.siteDataPath
  let json ← match Json.parse (← IO.FS.readFile path) with
    | .ok json => pure json
    | .error err => throwError "failed to parse {path}: {err}"
  let payload ← match FromJson.fromJson? (α := Payload) json with
    | .ok payload => pure payload
    | .error err => throwError "failed to decode {path}: {err}"
  unless payload.schemaVersion == expectedSchemaVersion do
    throwError "{path} has schema version {payload.schemaVersion}, expected \
{expectedSchemaVersion}; regenerate it with `lka site-data`"
  return payload

def loadPayload : TermElabM Payload := do
  if let some cached := ← payloadCache.get then return cached
  let payload ← readPayload
  payloadCache.set (some payload)
  return payload

private def loadModuleFile (path : System.FilePath) : TermElabM SubVerso.Module.Module := do
  let json ← match Json.parse (← IO.FS.readFile path) with
    | .ok json => pure json
    | .error err => throwError "failed to parse {path}: {err}"
  match FromJson.fromJson? (α := SubVerso.Module.Module) json with
  | .ok mod => return mod
  | .error err => throwError "failed to decode {path}: {err}"

def loadModuleSource (module : String) : TermElabM SubVerso.Module.Module :=
  loadModuleFile (Arena.siteSourcesDir / (module ++ ".json"))

def loadTestExport? (test : String) : TermElabM (Option SubVerso.Module.Module) := do
  let path := Arena.siteExportsDir / (test ++ ".json")
  if ← path.pathExists then loadModuleFile path else return none

abbrev ResultIndex := Arena.PairIndex ResultInfo

def currentCheckers (payload : Payload) : Array CheckerInfo :=
  payload.checkers.filter (·.current)

def revisionsOf (payload : Payload) (checker : String) : Array CheckerInfo :=
  (payload.checkers.filter (·.name == checker)).qsort (·.runAt > ·.runAt)

def indexOf (payload : Payload) : ResultIndex :=
  let current := payload.checkers.filter (·.current) |>.map (·.name)
  Arena.indexPairs (fun result => (result.checker, result.test))
    (payload.results.filter fun result => current.contains result.checker
      && (payload.checkers.any fun c =>
            c.current && c.name == result.checker && c.version == result.revision))

def findResult (index : ResultIndex) (checker test : String) : Option ResultInfo :=
  Std.HashMap.get? index (checker, test)

end ArenaSite
