import Arena.Build

namespace Arena

def Status.consoleGlyph : Status → String
  | .accepted => "accepted"
  | .rejected => "rejected"
  | .declined => "declined"
  | .error => "errored"

def Correctness.consoleGlyph : Correctness → String
  | .correct => "ok"
  | .incorrect => "WRONG"
  | .either => "either"
  | .declined => "declined"
  | .error => "error"

structure Job where
  checker : CheckerConfig
  pin : Pin
  test : LocatedTest

def Job.label (job : Job) : String :=
  s!"{job.checker.name}@{job.pin.id} on {job.test.name}"

def runnerName : IO String := do
  match ← IO.getEnv "ARENA_RUNNER" with
  | some name => return name
  | none => return (← capture "uname" #["-n"]).getD "unknown"

def declinedInAdvance : String :=
  "Declined by the checker's `declines` field; the checker was not run."

def runJob (job : Job) : IO ResultEntry := do
  let stamp ← timestamp
  let entry (attempt : Attempt) : ResultEntry :=
    { test := job.test.name, testHash := job.test.hash, attempt, runAt := stamp }
  if job.checker.declines? job.test.name then
    return entry (.declined declinedInAdvance)
  let ndjson := job.test.ndjsonPath
  if !(← ndjson.pathExists) then
    return entry (.skipped s!"no export at {ndjson}")
  let work := checkerWorkDir job.checker job.pin
  IO.FS.createDirAll work
  let outcome ← runShell job.checker.runCommand (cwd := work)
    (env := #[("IN", some (← IO.FS.realPath ndjson).toString)])
    (measurePerf := true)
  return {
    toMetrics := outcome.toMetrics
    test := job.test.name
    testHash := job.test.hash
    attempt := .ran outcome.exitCode.toNat outcome.stdout outcome.stderr
    runAt := stamp
  }

def emptyLog (checker : CheckerConfig) (pin : Pin) (runner : String) : ResultLog :=
  { checker := checker.name
    revision := pin.id
    recipeHash := digestString (checker.recipe pin)
    runner
    entries := #[] }

def isPending (log : ResultLog) (test : LocatedTest) : Bool :=
  match log.find? test.name with
  | some entry => entry.testHash != test.hash
  | none => true

def pendingJobs (checker : CheckerConfig) (pin : Pin) (tests : Array LocatedTest)
    (log : ResultLog) : Array Job :=
  (tests.filter (isPending log ·)).map fun test => { checker, pin, test }

def runRevision (checker : CheckerConfig) (pin : Pin) (tests : Array LocatedTest)
    (rerun : Bool) : IO (Array ResultEntry) := do
  let runner ← runnerName
  let fresh := emptyLog checker pin runner
  let stored ← loadResultLog checker.name pin.id fresh.recipeHash
  let everything := tests.map fun test => ({ checker, pin, test } : Job)
  let (log, jobs) :=
    match stored with
    | .none => (fresh, everything)
    | .otherRecipe _ => (fresh, everything)
    | .matching log =>
      if rerun then (log, everything) else (log, pendingJobs checker pin tests log)
  if jobs.isEmpty then return #[]
  if jobs.any (fun job => !job.checker.declines? job.test.name) then
    unless ← isBuilt checker pin do
      buildChecker checker pin
  let mut log := { log with runner }
  let mut produced := #[]
  for job in jobs do
    let separator := if ← verbose then "\n" else " "
    IO.print s!"Running {job.label}...{separator}"
    let entry ← runJob job
    log := log.record entry
    produced := produced.push entry
    writeResultLog log
    IO.println s!"[{entry.status.consoleGlyph} \
{(entry.correctness job.test.expectation).consoleGlyph} {formatDuration entry.wallTime}]"
  return produced

def expectationsOf (tests : Array LocatedTest) : Std.HashMap String (Option Expectation) :=
  tests.foldl (init := {}) fun map test => map.insert test.name test.expectation

def reportTally (tests : Array LocatedTest) (entries : Array ResultEntry) : IO Unit := do
  if entries.isEmpty then
    IO.println "\nNothing to run; every selected pair already has a result."
    return
  let expectations := expectationsOf tests
  let rule := "".pushn '=' 60
  IO.println s!"\n{rule}\nSummary:\n{rule}"
  for correctness in Correctness.all do
    let count := entries.countP fun entry =>
      entry.correctness (Std.HashMap.get? expectations entry.test |>.getD none) == correctness
    if count > 0 then
      note 1 s!"{correctness}: {count}"

end Arena
