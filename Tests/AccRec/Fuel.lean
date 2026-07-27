import Lean
open Lean Meta Elab Tactic

def r (x y : Nat) := x > y

def f (n : Nat) (a : Acc r n) : Bool :=
  Acc.rec (motive := fun _ _ => Bool)
    (fun x _ ih => ih (x + 1) (Nat.lt_succ_self x)) a

theorem ex1 (a : Acc r 0) :
    f 0 a = f 0 (Acc.intro 0 fun _ h => a.inv h) :=
  rfl

theorem ex2 (a : Acc r 0) :
    f 0 (Acc.intro 0 fun _ h => a.inv h) = f 1 (a.inv (Nat.lt_succ_self 0)) :=
  rfl

def srRedex (a : Acc r 0) (D : Bool → Type)
    (k : D (f 1 (a.inv (Nat.lt_succ_self 0))) → Unit) (x0 : D (f 0 a)) : Unit :=
  (fun y : D (f 0 (Acc.intro 0 fun _ h => a.inv h)) => k y) x0

set_option debug.skipKernelTC true

theorem bad (a : Acc r 0) :
    f 0 a = f 1 (a.inv (Nat.lt_succ_self 0)) := by
  run_tac closeMainGoalUsing `bogus fun goalType _ => do
    let some (ty, lhs, _) := goalType.eq? | throwError "goal is not an equality"
    return mkApp2 (mkConst ``Eq.refl [Level.one]) ty lhs

def srReduct (a : Acc r 0) (D : Bool → Type)
    (k : D (f 1 (a.inv (Nat.lt_succ_self 0))) → Unit) (x0 : D (f 0 a)) : Unit := by
  run_tac closeMainGoalUsing `bogus fun _ _ => do
    let lctx ← getLCtx
    let some kd := lctx.findFromUserName? `k | throwError "no k"
    let some xd := lctx.findFromUserName? `x0 | throwError "no x0"
    return mkApp kd.toExpr xd.toExpr
