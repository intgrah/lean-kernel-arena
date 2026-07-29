import Lean.Data.Json
import Arena.Verdict
import Arena.Layout

namespace Arena

def thinSpace : String := " " -- narrow no-break space U+202F

def fixed (digits : Nat) (x : Float) : String :=
  let scale := 10 ^ digits
  let n := (x * scale.toFloat).round.toUInt64.toNat
  if digits == 0 then
    ToString.toString n
  else
    let frac := ToString.toString (n % scale)
    s!"{n / scale}.{"".pushn '0' (digits - frac.length)}{frac}"

private def scaled (x : Float) (units : List (Float × String)) (fallback : String) : String :=
  match units.find? fun (threshold, _) => x ≥ threshold with
  | some (threshold, unit) => s!"{fixed 1 (x / threshold)}{thinSpace}{unit}"
  | none => s!"{fixed 0 x}{thinSpace}{fallback}"

def formatDuration (seconds : Float) : String :=
  if seconds ≥ 1 then scaled seconds [(3600, "h"), (60, "m"), (1, "s")] "s"
  else s!"{fixed 0 (seconds * 1000)}{thinSpace}ms"

def formatMemory (bytes : Float) : String :=
  scaled bytes [(1073741824, "GB"), (1048576, "MB"), (1024, "KB")] "B"

def formatUnitless (count : Float) : String :=
  if count < 1e3 then fixed 0 count
  else scaled count [(1e12, "T"), (1e9, "G"), (1e6, "M"), (1e3, "k")] ""

def selects (pattern name : String) : Bool :=
  pattern == name || (pattern.endsWith "/" && name.startsWith pattern)

def dropSuffix (suffix s : String) : String :=
  (s.toSlice.dropSuffix suffix).toString

abbrev PairIndex (α : Type) := Std.HashMap (String × String) α

def indexPairs (key : α → String × String) (items : Array α) : PairIndex α :=
  items.foldl (init := {}) fun index item => index.insert (key item) item

def relativeName (base path : System.FilePath) (ext : String) : Option String := do
  let base := base.toString
  let path := path.toString
  guard (path.startsWith base)
  let rest := path.drop base.length |>.dropWhile (· == '/')
  guard (rest.endsWith ext)
  return (rest.dropEnd ext.length).toString

private def sortedEntries (dir : System.FilePath) : IO (Array IO.FS.DirEntry) := do
  if !(← dir.isDir) then return #[]
  return (← dir.readDir).qsort (fun a b => a.fileName < b.fileName)

def findNamesIn (dir : System.FilePath) (ext : String) : IO (Array String) := do
  let entries ← sortedEntries dir
  return entries.filterMap fun entry => relativeName dir entry.path ext

partial def findNamesUnder (dir : System.FilePath) (ext : String) (depth : Nat) :
    IO (Array String) :=
  go dir depth
where
  go (d : System.FilePath) (remaining : Nat) : IO (Array String) := do
    let mut out := #[]
    for entry in ← sortedEntries d do
      if entry.fileName.startsWith "." then
        continue
      else if let some name := relativeName dir entry.path ext then
        out := out.push name
      else if remaining > 0 && (← entry.path.isDir) then
        out := out ++ (← go entry.path (remaining - 1))
    return out

def absolute (path : System.FilePath) : IO System.FilePath := do
  if path.isAbsolute then return path else return (← IO.currentDir) / path

def readJsonFile (path : System.FilePath) : IO Lean.Json := do
  match Lean.Json.parse (← IO.FS.readFile path) with
  | .ok json => return json
  | .error err => throw <| .userError s!"{path}: {err}"

def writeJsonFile (path : System.FilePath) (json : Lean.Json) : IO Unit := do
  if let some parent := path.parent then IO.FS.createDirAll parent
  IO.FS.writeFile path (json.pretty ++ "\n")

def removeIfExists (path : System.FilePath) : IO Unit := do
  if ← path.isDir then IO.FS.removeDirAll path
  else if ← path.pathExists then IO.FS.removeFile path

end Arena
