import ArenaSite.Render
import ArenaSite.Descriptions

open Lean
open Lean.Elab Term
open Verso Doc Verso.Genre.Blog
open Verso.Output Html
open SubVerso.Highlighting (Highlighted)

namespace ArenaSite.Pages.Details

open ArenaSite.Render
open ArenaSite.Copy

structure CheckerRow where
  test : TestInfo
  result : ResultInfo
  baseline : Option ResultInfo
deriving Inhabited, Quote

structure TestRow where
  checker : String
  result : ResultInfo
deriving Inhabited, Quote

def testDescriptionBlocks (test : TestInfo) : Array (Block Page) :=
  match Descriptions.testDescription? test.name with
  | some blocks => blocks
  | none => match test.generatedDescription with
    | some description => #[para #[text description]]
    | none => #[]

def checkerDescriptionBlocks (name : String) : Array (Block Page) :=
  (Descriptions.checkerDescription? name).getD #[]

private def expectationColumn (test : α → TestInfo) : Column α :=
  column columnExpected (align := .center) fun row =>
    match (test row).expectation with
    | some expectation =>
      {{ <td class="cell" title={{toString expectation}} data-sort={{toString expectation}}>
           {{textHtml (expectationGlyph expectation)}}</td> }}
    | none => {{ <td class="cell">{{textHtml missing}}</td> }}

private def testMetaRow (test : TestInfo) : Html :=
  let items := #[
    some (metaItem sizeLabel (Arena.formatMemory test.size.toFloat)),
    some (metaItem linesLabel (Arena.formatUnitless test.lines.toFloat)),
    test.exporterVersion.map (metaItem exporterLabel),
    test.leanVersion.map (metaItem leanLabel)
  ].filterMap id
  metaRow items (linkGroup test.declarationUrl test.sourceUrl)

private def attemptFact : Attempt → String
  | .ran exitCode _ _ => s!"{exitCodeLabel} {exitCode}"
  | .declined reason | .skipped reason => reason

private def resultLine (test : TestInfo) (result : ResultInfo) : Html :=
  let metrics := result.toMetrics
  let facts := #[
    some (attemptFact result.attempt),
    if metrics.wallTime > 0 then
      some s!"{wallTimeLabel}: {Arena.formatDuration metrics.wallTime}"
    else none,
    if metrics.instructions > 0 then
      some s!"{instructionsLabel}: {Arena.formatUnitless metrics.instructions.toFloat}"
    else none,
    if metrics.maxRss > 0 then
      some s!"{memoryLabel}: {Arena.formatMemory metrics.maxRss.toFloat}"
    else none
  ].filterMap id
  {{
    <p class="meta-row">
      <b>{{textHtml resultLabel}}</b>
      <span class={{(toneOf result.status test.expectation).className}}>
        {{textHtml s!"{statusGlyph result.status} {result.status}"}}
      </span>
      {{facts.map fun fact => {{ <span class="meta-item">{{textHtml fact}}</span> }} }}
    </p>
  }}

private def scoreCards (stats : CheckerStats) : Block Page :=
  blockHtml {{
    <div class="score-cards">
      {{scoreCard completenessCard (fraction stats.acceptCorrect stats.acceptTotal)}}
      {{scoreCard soundnessCard (fraction stats.rejectCorrect stats.rejectTotal)}}
      {{scoreCard declinedCard (toString stats.declined)}}
    </div>
  }}

private def detailBlocks (row : CheckerRow) : Array (Block Page) :=
  #[blockHtml {{
      <h3 id={{anchorId row.test.name}}>{{textHtml (testHeading row.test.name)}}</h3>
    }},
    blockHtml (testMetaRow row.test)]
  ++ testDescriptionBlocks row.test
  ++ #[blockHtml (resultLine row.test row.result),
       blockHtml (Html.fromArray (processOutput row.result.attempt))]

private def revisionsTable (revisions : Array CheckerInfo) (rate : Nat) : Block Page :=
  renderTable {
    nameLabel := columnRevision
    name := (·.version)
    href := fun _ => ""
    columns := #[
      column columnMeasured fun revision =>
        {{ <td class="version">{{textHtml (revision.runAt.take 10).toString}}</td> }},
      column completenessColumn (align := .center) fun revision =>
        fractionCell .incomplete revision.stats.acceptCorrect revision.stats.acceptTotal,
      column soundnessColumn (align := .center) fun revision =>
        fractionCell .unsound revision.stats.rejectCorrect revision.stats.rejectTotal,
      column declinedColumn (align := .center) fun revision =>
        {{ <td class="cell">{{textHtml (toString revision.stats.declined)}}</td> }},
      column timeColumn (align := .numeric) fun revision =>
        durationCell revision.stats.benchmark rate missing,
      column memoryColumn (align := .numeric) fun revision =>
        memoryCell revision.stats.benchmark missing
    ]
  } revisions

def checkerPart (checker : CheckerInfo) (revisions : Array CheckerInfo)
    (rows : Array CheckerRow) (rate : Nat) : Part Page :=
  let header := #[
    breadcrumb checker.name,
    heading (checkerHeading checker.name),
    blockHtml (metaRow #[metaItem versionLabel checker.version]
      (linkGroup checker.declarationUrl checker.sourceUrl))
  ]
  let summary := renderTable {
    nameLabel := columnTest
    name := (·.test.name)
    href := fun name => "#" ++ anchorId name
    columns :=
      #[expectationColumn (·.test), statusColumn columnResult (·.test) (·.result)]
      ++ perfColumns (·.test) (·.result) (·.baseline) rate
  } rows
  let body :=
    if rows.isEmpty then #[para #[text noResults]]
    else #[scoreCards checker.stats]
      ++ (if revisions.size ≤ 1 then #[]
          else #[heading revisionsHeading, revisionsTable revisions rate])
      ++ #[summary, heading detailedResultsHeading]
      ++ rows.flatMap detailBlocks
  pagePart checker.name (header ++ checkerDescriptionBlocks checker.name ++ body)

def codeBlocks (title : String) : Option Highlighted → Array (Block Page)
  | none => #[]
  | some code =>
    #[heading title, .other (BlockExt.highlightedCode { contextName := `arena } code) #[]]

def testPart (test : TestInfo) (rows : Array TestRow) (baseline : Option ResultInfo)
    (rate : Nat) (source exported : Option Highlighted) : Part Page :=
  let expectationItem :=
    match test.expectation with
    | some expectation =>
      metaItem expectedLabel s!"{expectationGlyph expectation} {expectation}"
    | none => metaItem expectedLabel missing
  let header := #[
    breadcrumb test.name,
    heading (testHeading test.name),
    blockHtml (metaRow
      #[expectationItem, metaItem sizeLabel (Arena.formatMemory test.size.toFloat),
        metaItem linesLabel (Arena.formatUnitless test.lines.toFloat)]
      (linkGroup test.declarationUrl test.sourceUrl))
  ]
  let table :=
    if rows.isEmpty then #[para #[text noCheckerResults]]
    else #[renderTable {
      nameLabel := columnChecker
      name := (·.checker)
      href := fun checker => resultHref checker test.name
      columns :=
        #[statusColumn columnResult (fun _ => test) (·.result)]
        ++ perfColumns (fun _ => test) (·.result) (fun _ => baseline) rate
    } rows]
  pagePart test.name (header ++ testDescriptionBlocks test ++ table
    ++ codeBlocks sourceHeading source ++ codeBlocks exportHeading exported)

def groupPart (title : String) (children : Array String) : Part Page :=
  pagePart title #[
    breadcrumb title,
    blockHtml {{
      <ul class="group-list">
        {{children.map fun name =>
          {{ <li><a href={{testHref name}}>{{textHtml name}}</a></li> }} }}
      </ul>
    }}
  ]

end ArenaSite.Pages.Details
