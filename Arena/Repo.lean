import Std.Time
import Arena.Results

namespace Arena

def repoUrl : String := "https://github.com/leanprover/lean-kernel-arena"

structure BuildInfo where
  timestamp : String
  gitRevision : Option String
  commitUrl : Option String
  actionUrl : Option String
  actionRunId : Option String

def BuildInfo.shortRevision (info : BuildInfo) : Option String :=
  info.gitRevision.map (·.take 8 |>.toString)

private def commitUrlOf (remote revision : String) : Option String := do
  guard ((remote.splitOn "github.com").length == 2)
  let path := dropSuffix ".git" ((remote.splitOn "github.com").getLast!.drop 1).toString
  return s!"https://github.com/{path}/commit/{revision}"

private def actionRun : IO (Option (String × String)) := do
  let some server ← IO.getEnv "GITHUB_SERVER_URL" | return none
  let some repository ← IO.getEnv "GITHUB_REPOSITORY" | return none
  let some runId ← IO.getEnv "GITHUB_RUN_ID" | return none
  return some (s!"{server}/{repository}/actions/runs/{runId}", runId)

def buildInfo : IO BuildInfo := do
  let now := Std.Time.DateTime.ofTimestampWithZone (← Std.Time.Timestamp.now) .UTC
  let gitRevision ← capture "git" #["rev-parse", "HEAD"]
  let remote ← capture "git" #["remote", "get-url", "origin"]
  let action ← actionRun
  return {
    timestamp := now.format "yyyy-MM-dd HH:mm:ss" ++ " UTC"
    gitRevision
    commitUrl := do commitUrlOf (← remote) (← gitRevision)
    actionUrl := action.map (·.1)
    actionRunId := action.map (·.2)
  }

structure SourceLinks where
  declarationUrl : Option String
  sourceUrl : Option String

private def treeUrl (path : String) (revision : String) : String :=
  s!"{repoUrl}/tree/{revision}/{path}"

private def blobUrl (path : String) (revision : String) : String :=
  s!"{repoUrl}/blob/{revision}/{path}"

private def externalUrl (url : String) (rev : Option String) : String :=
  match rev with
  | some rev =>
    if (url.splitOn "github.com").length == 2 then
      let base := dropSuffix "/" (dropSuffix ".git" url)
      s!"{base}/tree/{rev}"
    else url
  | none => url

def sourceLinks (configPath : String) (source : Source) (dirPrefix : String)
    (revision : Option String) : SourceLinks :=
  match revision with
  | none => { declarationUrl := none, sourceUrl := none }
  | some revision =>
    { declarationUrl := some (blobUrl configPath revision)
      sourceUrl := match source with
        | .git url _ rev => some (externalUrl url rev)
        | .localDir path => some (treeUrl (dirPrefix ++ path) revision)
        | .leanFile path => some (blobUrl path revision)
        | .empty => none }

def TestConfig.configPath (config : TestConfig) : String :=
  s!"tests/{config.name}.toml"

def CheckerConfig.configPath (config : CheckerConfig) : String :=
  s!"checkers/{config.name}.toml"

def TestConfig.sourceLinks (config : TestConfig) (revision : Option String) : SourceLinks :=
  Arena.sourceLinks config.configPath config.source "" revision

def CheckerConfig.sourceLinks (config : CheckerConfig) (revision : Option String) : SourceLinks :=
  Arena.sourceLinks config.configPath config.source "checkers/" revision

end Arena
