import Lean
open Lean Elab Command

-- Inject `natAddLitBad : Nat.add a b = c` with value `Eq.refl (Nat.add a b)`
-- where `c` is the wrong literal. The elaborator never sees the mismatch;
-- `debug.skipKernelTC` lets `addDecl` write it into the export.

run_cmd liftTermElabM do
  let lhs := mkApp2 (mkConst ``Nat.add) (mkNatLit 123456789) (mkNatLit 987654321)
  let rhs := mkNatLit 1111111111
  let ty := mkApp3 (mkConst ``Eq [1]) (mkConst ``Nat) lhs rhs
  let val := mkApp2 (mkConst ``Eq.refl [1]) (mkConst ``Nat) lhs
  let decl : Declaration := .thmDecl {
    name := `natAddLitBad
    levelParams := []
    type := ty
    value := val
  }
  withOptions (debug.skipKernelTC.set · true) do
    addDecl decl
