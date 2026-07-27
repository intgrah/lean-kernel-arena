import Arena.Build

namespace Arena

def Status.glyph : Status → String
  | .accepted => "👍"
  | .rejected => "👎"
  | .declined => "⊘"
  | .error => "⚠️"

def Correctness.glyph : Correctness → String
  | .correct => "✅"
  | .incorrect => "❌"
  | .declined => "⊘"
  | .error => "⚠️"

private def missingTestResult (checker : String) (test : TestStats) : RunResult :=
  { checker
    test := test.name
    status := .error
    correctness := .error
    exitCode := -1
    message := some s!"test file not found: {test.ndjsonPath}" }

def runCheckerOnTest (checker : CheckerConfig) (test : TestStats) : IO RunResult := do
  let ndjson := test.ndjsonPath
  unless ← ndjson.pathExists do
    let result := missingTestResult checker.name test
    writeRunResult result
    return result
  let work := checkerWorkDir checker
  IO.FS.createDirAll work
  let outcome ← runShell checker.runCommand (cwd := work)
    (env := #[("IN", (← IO.FS.realPath ndjson).toString)]) (measurePerf := true)
  let status := Status.ofExitCode outcome.exitCode
  let result : RunResult := {
    toMetrics := outcome.toMetrics
    checker := checker.name
    test := test.name
    status
    correctness := Correctness.of status test.expectation
    exitCode := outcome.exitCode.toNat
    stdout := outcome.stdout
    stderr := outcome.stderr
  }
  writeRunResult result
  return result

structure Tally where
  correct : Nat := 0
  incorrect : Nat := 0
  declined : Nat := 0
  error : Nat := 0

def Tally.record (tally : Tally) : Correctness → Tally
  | .correct => { tally with correct := tally.correct + 1 }
  | .incorrect => { tally with incorrect := tally.incorrect + 1 }
  | .declined => { tally with declined := tally.declined + 1 }
  | .error => { tally with error := tally.error + 1 }

def Tally.report (tally : Tally) : IO Unit := do
  IO.println ("\n" ++ "".pushn '=' 60)
  IO.println "Summary:"
  IO.println ("".pushn '=' 60)
  for (label, count, correctness) in
      [("correct", tally.correct, Correctness.correct),
       ("incorrect", tally.incorrect, .incorrect),
       ("declined", tally.declined, .declined),
       ("error", tally.error, .error)] do
    if count > 0 then
      IO.println s!"  {label}: {count} {correctness.glyph}"

def runCheckers (checkers : Array CheckerConfig) (tests : Array TestStats) : IO Tally := do
  let mut tally : Tally := {}
  for checker in checkers do
    for test in tests do
      let separator := if ← verbose then "\n" else " "
      IO.print s!"Running {checker.name} on {test.name}...{separator}"
      let result ← runCheckerOnTest checker test
      tally := tally.record result.correctness
      IO.println s!"[{result.status.glyph} {result.correctness.glyph} \
{formatDuration result.wallTime}]"
  return tally

end Arena
