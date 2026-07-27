import ArenaSite.Render
import ArenaSite.Descriptions

open Lean
open Lean.Elab Term
open Verso Doc Verso.Genre.Blog
open Verso.Output Html

namespace ArenaSite.Pages.Details

open ArenaSite.Render
open ArenaSite.Copy

structure CheckerRow where
  test : TestInfo
  result : ResultInfo
  official : Option ResultInfo
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

private def durationCell (result : ResultInfo) (rate : Nat) : Html :=
  match virtualSeconds result.metrics rate with
  | some seconds =>
    let hint := Arena.formatInstructions result.metrics.instructions.toFloat ++ " instructions"
    {{ <td class="numeric" title={{hint}}>{{textHtml (Arena.formatDuration seconds)}}</td> }}
  | none => {{ <td class="numeric"></td> }}

private def durationDeltaCell (test : TestInfo) (result : ResultInfo)
    (official : Option ResultInfo) (rate : Nat) : Html :=
  let delta : Option Int := do
    guard (perfComparable test result.status)
    let official ← official
    guard (official.status == .accepted)
    let baseline ← virtualSeconds official.metrics rate
    guard (baseline ≥ timeComparisonFloor)
    let seconds ← virtualSeconds result.metrics rate
    return percentChange baseline seconds
  match delta with
  | some percent => {{ <td class="delta">{{textHtml s!"({formatPercent percent})"}}</td> }}
  | none => {{ <td class="delta"></td> }}

private def memoryCell (result : ResultInfo) : Html :=
  if result.metrics.maxRss > 0 then
    {{ <td class="numeric">{{textHtml (Arena.formatMemory result.metrics.maxRss.toFloat)}}</td> }}
  else
    {{ <td class="numeric"></td> }}

private def memoryDeltaCell (test : TestInfo) (result : ResultInfo)
    (official : Option ResultInfo) : Html :=
  let delta : Option Int := do
    guard (perfComparable test result.status)
    let official ← official
    guard (official.status == .accepted)
    guard (official.metrics.maxRss ≥ memoryComparisonFloor)
    guard (result.metrics.maxRss > 0)
    return percentChange official.metrics.maxRss.toFloat result.metrics.maxRss.toFloat
  match delta with
  | some percent => {{ <td class="delta">{{textHtml s!"({formatPercent percent})"}}</td> }}
  | none => {{ <td class="delta"></td> }}

def perfCells (test : TestInfo) (result : ResultInfo) (official : Option ResultInfo)
    (rate : Nat) : Array Html :=
  #[durationCell result rate, durationDeltaCell test result official rate,
    memoryCell result, memoryDeltaCell test result official]

def perfHeaderCells : Array Html :=
  #[{{ <th class="numeric">{{textHtml timeGlyph}}</th> }}, {{ <th class="delta"></th> }},
    {{ <th class="numeric">{{textHtml memoryGlyph}}</th> }}, {{ <th class="delta"></th> }}]

private def expectationCell (expectation : Option Expectation) : Html :=
  match expectation with
  | some expectation =>
    {{ <td class="cell" title={{expectationLabel expectation}}>
         {{textHtml (expectationGlyph expectation)}}</td> }}
  | none => {{ <td class="cell">{{textHtml missing}}</td> }}

private def statusOnlyCell (test : TestInfo) (result : ResultInfo) : Html :=
  let tone := toneOf result.status test.expectation
  {{ <td class={{tone.className}} title={{statusLabel result.status}}>
       {{textHtml (statusGlyph result.status)}}</td> }}

private def testMetaRow (test : TestInfo) : Html :=
  let items := #[
    some (metaItem sizeLabel (Arena.formatMemory test.size.toFloat)),
    some (metaItem linesLabel (Arena.formatUnitless test.lines.toFloat)),
    test.exporterVersion.map (metaItem exporterLabel),
    test.leanVersion.map (metaItem leanLabel)
  ].filterMap id
  metaRow items (linkGroup test.declarationUrl test.sourceUrl)

private def resultLine (test : TestInfo) (result : ResultInfo) : Html :=
  let tone := toneOf result.status test.expectation
  let facts := #[
    some s!"{exitCodeLabel} {result.exitCode}",
    if result.metrics.wallSeconds > 0 then
      some s!"{wallTimeLabel}: {Arena.formatDuration result.metrics.wallSeconds}"
    else none,
    if result.metrics.instructions > 0 then
      some s!"{instructionsLabel}: {Arena.formatUnitless result.metrics.instructions.toFloat}"
    else none,
    if result.metrics.maxRss > 0 then
      some s!"{memoryLabel}: {Arena.formatMemory result.metrics.maxRss.toFloat}"
    else none
  ].filterMap id
  {{
    <p class="meta-row">
      <b>{{textHtml resultLabel}}</b>
      <span class={{tone.className}}>
        {{textHtml s!"{statusGlyph result.status} {statusLabel result.status}"}}
      </span>
      {{facts.map fun fact => {{ <span class="meta-item">{{textHtml fact}}</span> }} }}
    </p>
  }}

private def checkerSummaryRow (row : CheckerRow) (rate : Nat) : Html :=
  {{
    <tr>
      <td class="name">
        <a href={{"#" ++ anchorId row.test.name}}>{{textHtml row.test.name}}</a>
      </td>
      {{expectationCell row.test.expectation}}
      {{statusOnlyCell row.test row.result}}
      {{perfCells row.test row.result row.official rate}}
    </tr>
  }}

private def checkerSummaryTable (rows : Array CheckerRow) (rate : Nat) : Block Page :=
  blockHtml <| scrollTable {{
    <table class="arena-table">
      <thead>
        <tr>
          <th>{{textHtml columnTest}}</th>
          <th class="center">{{textHtml columnExpected}}</th>
          <th class="center">{{textHtml columnResult}}</th>
          {{perfHeaderCells}}
        </tr>
      </thead>
      <tbody>{{rows.map (checkerSummaryRow · rate)}}</tbody>
    </table>
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
       blockHtml (Html.fromArray (preBlock stdoutLabel row.result.stdout
         ++ preBlock stderrLabel row.result.stderr))]

def checkerPart (checker : CheckerInfo) (rows : Array CheckerRow) (rate : Nat) : Part Page :=
  let header := #[
    breadcrumb checker.name,
    heading (checkerHeading checker.name),
    blockHtml (metaRow #[metaItem versionLabel checker.version]
      (linkGroup checker.declarationUrl checker.sourceUrl))
  ]
  let body :=
    if rows.isEmpty then #[para #[text noResults]]
    else
      #[scoreCards checker.stats, checkerSummaryTable rows rate,
        blockHtml {{ <h2>{{textHtml detailedResultsHeading}}</h2> }}]
      ++ rows.flatMap detailBlocks
  pagePart checker.name (header ++ checkerDescriptionBlocks checker.name ++ body)

private def testResultRow (test : TestInfo) (row : TestRow) (official : Option ResultInfo)
    (rate : Nat) : Html :=
  {{
    <tr>
      <td class="name">
        <a href={{resultHref row.checker test.name}}>{{textHtml row.checker}}</a>
      </td>
      {{statusOnlyCell test row.result}}
      {{perfCells test row.result official rate}}
    </tr>
  }}

def testPart (test : TestInfo) (rows : Array TestRow) (official : Option ResultInfo)
    (rate : Nat) : Part Page :=
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
    else #[blockHtml <| scrollTable {{
      <table class="arena-table">
        <thead>
          <tr>
            <th>{{textHtml columnChecker}}</th>
            <th class="center">{{textHtml columnResult}}</th>
            {{perfHeaderCells}}
          </tr>
        </thead>
        <tbody>{{rows.map (testResultRow test · official rate)}}</tbody>
      </table>
    }}]
  pagePart test.name (header ++ testDescriptionBlocks test ++ table)
where
  expectationItem :=
    match test.expectation with
    | some expectation =>
      metaItem expectedLabel s!"{expectationGlyph expectation} {expectationLabel expectation}"
    | none => metaItem expectedLabel missing

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
