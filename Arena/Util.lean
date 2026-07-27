import Lean.Data.Json

namespace Arena

def thinSpace : String := " " -- narrow no-break space U+202F

private def roundTo (digits : Nat) (x : Float) : Nat :=
  let scale := (10 ^ digits).toFloat
  (x * scale + 0.5).floor.toUInt64.toNat

def fixed (digits : Nat) (x : Float) : String :=
  if digits == 0 then
    toString (roundTo 0 x)
  else
    let n := roundTo digits x
    let scale := 10 ^ digits
    let frac := toString (n % scale)
    s!"{n / scale}.{"".pushn '0' (digits - frac.length)}{frac}"

def formatDuration (seconds : Float) : String :=
  if seconds ≥ 3600 then s!"{fixed 1 (seconds / 3600)}{thinSpace}h"
  else if seconds ≥ 60 then s!"{fixed 1 (seconds / 60)}{thinSpace}m"
  else if seconds ≥ 1 then s!"{fixed 1 seconds}{thinSpace}s"
  else s!"{fixed 0 (seconds * 1000)}{thinSpace}ms"

def formatMemory (bytes : Float) : String :=
  if bytes ≥ 1073741824 then s!"{fixed 1 (bytes / 1073741824)}{thinSpace}GB"
  else if bytes ≥ 1048576 then s!"{fixed 1 (bytes / 1048576)}{thinSpace}MB"
  else if bytes ≥ 1024 then s!"{fixed 1 (bytes / 1024)}{thinSpace}KB"
  else s!"{fixed 0 bytes}{thinSpace}B"

def formatUnitless (count : Float) : String :=
  if count ≥ 1e12 then s!"{fixed 1 (count / 1e12)}{thinSpace}T"
  else if count ≥ 1e9 then s!"{fixed 1 (count / 1e9)}{thinSpace}G"
  else if count ≥ 1e6 then s!"{fixed 1 (count / 1e6)}{thinSpace}M"
  else if count ≥ 1e3 then s!"{fixed 1 (count / 1e3)}{thinSpace}k"
  else toString (roundTo 0 count)

def formatInstructions (count : Float) : String :=
  if count ≥ 1e9 then s!"{fixed 1 (count / 1e9)}{thinSpace}G"
  else if count ≥ 1e6 then s!"{fixed 1 (count / 1e6)}{thinSpace}M"
  else if count ≥ 1e3 then s!"{fixed 1 (count / 1e3)}{thinSpace}k"
  else toString (roundTo 0 count)

def instructionsToTime (instructions : Nat) (perSecond : Float) : Float :=
  if perSecond > 0 && instructions > 0 then instructions.toFloat / perSecond else 0.0

partial def globMatch (pattern name : String) : Bool :=
  go pattern.toList name.toList
where
  go : List Char → List Char → Bool
    | [], cs => cs.isEmpty
    | '*' :: p, cs => go p cs || (!cs.isEmpty && go ('*' :: p) cs.tail)
    | '?' :: p, _ :: cs => go p cs
    | '[' :: p, c :: cs =>
      let (negated, p) := match p with
        | '!' :: rest | '^' :: rest => (true, rest)
        | rest => (false, rest)
      let (set, rest) := p.span (· != ']')
      go rest.tail! cs && (set.contains c != negated)
    | q :: p, c :: cs => q == c && go p cs
    | _ :: _, [] => false

def dropSuffix (suffix s : String) : String :=
  if s.endsWith suffix then (s.dropEnd suffix.length).toString else s

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

partial def findNamesUnder (dir : System.FilePath) (ext : String) : IO (Array String) :=
  go dir
where
  go (d : System.FilePath) : IO (Array String) := do
    let mut out := #[]
    for entry in ← sortedEntries d do
      if entry.fileName.startsWith "." then
        continue
      else if ← entry.path.isDir then
        out := out ++ (← go entry.path)
      else if let some name := relativeName dir entry.path ext then
        out := out.push name
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
