import Lake.Build.Trace
import Std.Time
import Arena.Proc

namespace Arena

def digestFile (path : System.FilePath) : IO String := do
  let outcome ← run { cmd := "sha256sum", args := #["--", path.toString] }
  unless outcome.ok do throw <| .userError s!"sha256sum failed for {path}: {outcome.stderr}"
  match outcome.stdout.trimAscii.toString.splitOn " " with
  | digest :: _ => return digest
  | [] => throw <| .userError s!"sha256sum produced no output for {path}"

def digestString (text : String) : String :=
  (Lake.Hash.ofString text).toString

def timestamp : IO String := do
  let now := Std.Time.DateTime.ofTimestampWithZone (← Std.Time.Timestamp.now) .UTC
  return now.format "yyyy-MM-dd'T'HH:mm:ss'Z'"

end Arena
