import Lake
open Lake DSL

require Cli from git "https://github.com/leanprover/lean4-cli" @ "v4.32.0"
require verso from git "https://github.com/leanprover/verso" @ "v4.32.0"

package arena where
  leanOptions := #[⟨`weak.linter.missingDocs, false⟩]

input_file siteData where
  path := "site-data/arena.json"
  text := true

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
