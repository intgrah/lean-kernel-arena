import Lean
open Lean Elab Command

run_cmd liftTermElabM do
  let decl : Declaration := .defnDecl {
      name := `d
      levelParams := []
      type  := .forallE `x (mkConst ``False) (mkConst ``False) .default
      value := .lam `x (mkConst ``False) (.proj `False 0 (.bvar 0)) .default
      hints := .abbrev
      safety := .safe
    }
  withOptions (debug.skipKernelTC.set · true) do
    addDecl decl
