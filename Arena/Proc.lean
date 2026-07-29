import Lake.Build.Trace
import Arena.Util

open Lean (Json ToJson FromJson)

namespace Arena

initialize verboseRef : IO.Ref Bool ← IO.mkRef false

def verbose : IO Bool := verboseRef.get

structure Metrics where
  wallTime : Float := 0
  cpuTime : Float := 0
  maxRss : Nat := 0
  instructions : Nat := 0
deriving Inhabited, ToJson, FromJson

structure Outcome extends Metrics where
  exitCode : UInt32
  stdout : String
  stderr : String
deriving Inhabited

def Outcome.ok (o : Outcome) : Bool := o.exitCode == 0

structure Invocation where
  cmd : String
  args : Array String := #[]
  cwd : Option System.FilePath := none
  env : Array (String × Option String) := #[]
  captureOutput : Bool := true

def Invocation.shell (script : String) : Invocation :=
  { cmd := "sh", args := #["-c", script] }

def Invocation.display (inv : Invocation) : String :=
  match inv.cmd, inv.args.toList with
  | "sh", ["-c", script] => script
  | cmd, args => " ".intercalate (cmd :: args)

def indent : String := "  "

private def indented (depth : Nat) (text : String) : String :=
  let pad := String.join (List.replicate depth indent)
  pad ++ ("\n" ++ pad).intercalate (text.splitOn "\n")

def note (depth : Nat) (message : String) : IO Unit :=
  IO.println (indented depth message)

def trace (depth : Nat) (message : String) : IO Unit := do
  if ← verbose then note depth message

private def spawnCapturing (inv : Invocation) : IO (UInt32 × String × String) := do
  if inv.captureOutput then
    let out ← IO.Process.output {
      cmd := inv.cmd, args := inv.args, cwd := inv.cwd, env := inv.env
    }
    return (out.exitCode, out.stdout, out.stderr)
  else
    let child ← IO.Process.spawn {
      cmd := inv.cmd, args := inv.args, cwd := inv.cwd, env := inv.env
    }
    return (← child.wait, "", "")

private def timed (inv : Invocation) : IO Outcome := do
  let startNanos ← IO.monoNanosNow
  let (exitCode, stdout, stderr) ← spawnCapturing inv
  let elapsed := ((← IO.monoNanosNow) - startNanos).toFloat / 1e9
  return { exitCode, stdout, stderr, wallTime := elapsed }

private def asFloat? : Json → Option Float
  | .num n => some n.toFloat
  | .str s => match Json.parse s with
    | .ok (.num n) => some n.toFloat
    | _ => none
  | _ => none

private def eventName (event : String) : String :=
  let unqualified := match event.splitOn "/" with
    | [_, name, _] => name
    | _ => event
  match unqualified.splitOn ":" with
    | base :: _ => base
    | [] => unqualified

private def parsePerfReport (text : String) : Metrics :=
  text.splitOn "\n" |>.foldl (init := {}) fun metrics line =>
    match Json.parse line.trimAscii.toString with
    | .error _ => metrics
    | .ok json =>
      let event? := (json.getObjValAs? String "event").toOption
      let value? := (json.getObjVal? "counter-value").toOption.bind asFloat?
      match event?, value? with
      | some event, some value =>
        let unit := (json.getObjValAs? String "unit").toOption.getD ""
        let seconds := if unit == "msec" then value * 1e-3 else value * 1e-9
        match eventName event with
        | "duration_time" => { metrics with wallTime := seconds }
        | "task-clock" => { metrics with cpuTime := seconds }
        | "instructions" =>
          { metrics with instructions := metrics.instructions + value.toUInt64.toNat }
        | _ => metrics
      | _, _ => metrics

private def parseMaxRssBytes (gnuTimeReport : String) : Nat :=
  gnuTimeReport.splitOn "\n" |>.foldl (init := 0) fun rss line =>
    match line.trimAscii.toString.splitOn "=" with
    | ["max_rss_kb", value] => (value.toNat?.getD 0) * 1024
    | _ => rss

private def measured (inv : Invocation) : IO Outcome := do
  IO.FS.withTempDir fun tmp => do
    let perfPath := tmp / "perf.json"
    let timePath := tmp / "time.txt"
    let timeFormat := "real_seconds=%e\nuser_seconds=%U\nsys_seconds=%S\nmax_rss_kb=%M"
    let wrapper := #[
      "stat", "-j", "-o", perfPath.toString,
      "-e", "duration_time", "-e", "task-clock", "-e", "instructions", "--",
      "time", "-f", timeFormat, "-o", timePath.toString, "--",
      inv.cmd
    ]
    let localeIndependent := inv.env.push ("LC_ALL", some "C")
    let outcome ← timed {
      inv with cmd := "perf", args := wrapper ++ inv.args, env := localeIndependent
    }
    let perf := parsePerfReport (← IO.FS.readFile perfPath)
    let maxRss := parseMaxRssBytes (← IO.FS.readFile timePath)
    let wallTime := if perf.wallTime > 0 then perf.wallTime else outcome.wallTime
    return { outcome with
      cpuTime := perf.cpuTime, instructions := perf.instructions, wallTime, maxRss }

private def Outcome.summary (outcome : Outcome) : String :=
  let status := if outcome.ok then "ok" else s!"exit {outcome.exitCode}"
  let measurements := #[
    (true, s!"wall {formatDuration outcome.wallTime}"),
    (outcome.cpuTime > 0, s!"cpu {formatDuration outcome.cpuTime}"),
    (outcome.maxRss > 0, s!"rss {formatMemory outcome.maxRss.toFloat}"),
    (outcome.instructions > 0, s!"inst {formatUnitless outcome.instructions.toFloat}")
  ]
  let shown := measurements.filterMap fun (keep, text) => if keep then some text else none
  s!"{status}, {", ".intercalate shown.toList}"

def run (inv : Invocation) (measurePerf := false) (printOnFailure := false) : IO Outcome := do
  let where? := match inv.cwd with | some dir => s!" (in {dir})" | none => ""
  trace 2 s!"$ {inv.display}{where?}"
  let outcome ← if measurePerf then measured inv else timed inv
  trace 3 outcome.summary
  if (← verbose) || (printOnFailure && !outcome.ok) then
    for (stream, text) in [("stdout", outcome.stdout), ("stderr", outcome.stderr)] do
      unless text.isEmpty do IO.println (indented 3 s!"{stream}: {text}")
  return outcome

def runShell (script : String) (cwd : Option System.FilePath := none)
    (env : Array (String × Option String) := #[]) (printOnFailure := false)
    (measurePerf := false) : IO Outcome :=
  run { Invocation.shell script with cwd, env } measurePerf printOnFailure

def capture (cmd : String) (args : Array String) (cwd : Option System.FilePath := none) :
    IO (Option String) := do
  let outcome ← run { cmd, args, cwd }
  return if outcome.ok then some outcome.stdout.trimAscii.toString else none

def digestFile (path : System.FilePath) : IO String := do
  let outcome ← run { cmd := "sha256sum", args := #["--", path.toString] }
  unless outcome.ok do throw <| .userError s!"sha256sum failed for {path}: {outcome.stderr}"
  match outcome.stdout.trimAscii.toString.splitOn " " with
  | digest :: _ => return digest
  | [] => throw <| .userError s!"sha256sum produced no output for {path}"

def digestString (text : String) : String :=
  (Lake.Hash.ofString text).toString

def copyFile (src dst : System.FilePath) : IO Unit := do
  if let some parent := dst.parent then IO.FS.createDirAll parent
  IO.FS.writeBinFile dst (← IO.FS.readBinFile src)

def linkFile (src dst : System.FilePath) : IO Unit := do
  if let some parent := dst.parent then IO.FS.createDirAll parent
  let outcome ← run { cmd := "cp", args := #["-l", src.toString, dst.toString] }
  unless outcome.ok do copyFile src dst

def copyTree (src dst : System.FilePath) : IO Unit := do
  if let some parent := dst.parent then IO.FS.createDirAll parent
  let outcome ← run { cmd := "cp", args := #["-a", src.toString, dst.toString] }
  unless outcome.ok do
    throw <| .userError s!"failed to copy {src} to {dst}: {outcome.stderr}"

end Arena
