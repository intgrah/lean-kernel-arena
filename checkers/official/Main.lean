import Export.Parse
import Lean.Replay
import Lean

def runKernel (solution : Export.ExportedEnv) : IO Unit := do
  let env ← Lean.mkEmptyEnvironment
  let constMap := solution.constMap
  discard <| env.replay constMap
  IO.println s!"Accepted {constMap.size} declarations."


def main (args : List String) : IO Unit := do
  let (inputPath, parseOnly) ← match args with
    | ["--parse-only", inputPath] => pure (inputPath, true)
    | [inputPath] => pure (inputPath, false)
    | _ => throw <| .userError "Expected input file path as first argument, optionally followed by --parse-only."
  let handle ← IO.FS.Handle.mk inputPath .read
  let env ← Export.parseStream (.ofHandle handle)
  if parseOnly then
    IO.println "Parse successful."
  else
    runKernel env
