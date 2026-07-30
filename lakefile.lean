import Lake
open Lake DSL

require Cli from git "https://github.com/leanprover/lean4-cli" @ "v4.32.0"
require verso from git "https://github.com/leanprover/verso" @ "v4.32.0"
require Lean4Export from git "https://github.com/leanprover/lean4export" @ "joachim/export-eq-with-qout"

package arena where
  leanOptions := #[⟨`weak.linter.missingDocs, false⟩]

input_dir siteData where
  path := "site-data"
  text := true

lean_lib Tests where
  srcDir := "Tests"
  roots := #[`Bogus1, `CtorNumFields, `InitModule, `InitPreludeModule, `KRecConv, `ProjNoConstructors, `ProjNonStructure, `ProjOfProp, `ProofIrrel, `RecKLie, `StdModule, `NestedNonuniformParam, `Undecidability.AlgConv, `Undecidability.AlgConvQuot, `Perf.AppLam, `Perf.GrindRing5, `Perf.ShiftCascade, `Tutorial]

lean_lib Arena

lean_lib ArenaSite where
  needs := #[siteData]

@[default_target]
lean_exe lka where
  root := `Main
  supportInterpreter := true

lean_exe «arena-site» where
  root := `ArenaSite
  supportInterpreter := true
