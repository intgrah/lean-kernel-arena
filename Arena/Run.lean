import Arena.Build

namespace Arena

def Status.consoleGlyph : Status → String
  | .accepted => "👍"
  | .rejected => "👎"
  | .declined => "⊘"
  | .error => "⚠️"

def Correctness.consoleGlyph : Correctness → String
  | .correct => "✅"
  | .incorrect => "❌"
  | .declined => "⊘"
  | .error => "⚠️"

private def missingTestResult (checker : String) (test : TestStats) : RunResult :=
  { checker
    test := test.name
    exitCode := -1
    message := some s!"test file not found: {test.ndjsonPath}" }

def runCheckerOnTest (checker : CheckerConfig) (test : TestStats) : IO RunResult := do
  let ndjson := test.ndjsonPath
  let result ←
    if !(← ndjson.pathExists) then
      pure (missingTestResult checker.name test)
    else do
      let work := checkerWorkDir checker
      IO.FS.createDirAll work
      let outcome ← runShell checker.runCommand (cwd := work)
        (env := #[("IN", (← IO.FS.realPath ndjson).toString)]) (measurePerf := true)
      pure {
        toMetrics := outcome.toMetrics
        checker := checker.name
        test := test.name
        exitCode := outcome.exitCode.toNat
        stdout := outcome.stdout
        stderr := outcome.stderr
      }
  writeRunResult result test.expectation
  return result

def runCheckers (checkers : Array CheckerConfig) (tests : Array TestStats) :
    IO (Array RunResult) := do
  let mut results := #[]
  for checker in checkers do
    for test in tests do
      let separator := if ← verbose then "\n" else " "
      IO.print s!"Running {checker.name} on {test.name}...{separator}"
      let result ← runCheckerOnTest checker test
      results := results.push result
      IO.println s!"[{result.status.consoleGlyph} \
{(result.correctness test.expectation).consoleGlyph} {formatDuration result.wallTime}]"
  return results

def expectationsOf (tests : Array TestStats) : Std.HashMap String (Option Expectation) :=
  tests.foldl (init := {}) fun map test => map.insert test.name test.expectation

def reportTally (tests : Array TestStats) (results : Array RunResult) : IO Unit := do
  let expectations := expectationsOf tests
  let rule := "".pushn '=' 60
  IO.println s!"\n{rule}\nSummary:\n{rule}"
  for correctness in Correctness.all do
    let count := results.countP fun result =>
      result.correctness (Std.HashMap.get? expectations result.test |>.getD none) == correctness
    if count > 0 then
      IO.println s!"  {correctness}: {count} {correctness.consoleGlyph}"

end Arena
