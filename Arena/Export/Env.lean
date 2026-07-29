module
public import Lean.Environment
import all Lean.Environment

open Lean

/--
Inserts `ConstantInfo`s directly into the environment, bypassing the kernel.

The arena's tests are deliberately malformed exports, so they have to reach the pretty printer
without being checked. `set_option debug.skipKernelTC true` does not cover adding inductives,
which several tests depend on, so the constant maps are written to directly.
-/
public def Arena.Export.addConstInfos {m : Type → Type} [Monad m] [MonadEnv m]
    (cis : Array ConstantInfo) : m Unit := do
  for ci in cis do
    modifyEnv fun env => { env with
      base.public.constants.map₁ := env.base.public.constants.map₁.insert ci.name ci
      base.private.constants.map₁ := env.base.private.constants.map₁.insert ci.name ci
      checked := env.checked.map fun e => { e with constants := e.constants.insert ci.name ci }
    }
