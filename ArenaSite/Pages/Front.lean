import ArenaSite.Render

open Lean
open Lean.Elab Term
open Verso Doc Verso.Genre.Blog
open Verso.Output Html

namespace ArenaSite.Pages.Front

open ArenaSite.Render
open ArenaSite.Copy

abbrev ResultIndex := Std.HashMap String ResultInfo

private def key (checker test : String) : String := checker ++ " " ++ test

def indexOf (payload : Payload) : ResultIndex :=
  payload.results.foldl (init := {}) fun index result =>
    index.insert (key result.checker result.test) result

def findResult (index : ResultIndex) (checker test : String) : Option ResultInfo :=
  Std.HashMap.get? index (key checker test)

private def seriousCheckers (payload : Payload) : Array CheckerInfo :=
  let serious := payload.checkers.filter (·.serious)
  serious.filter (·.name == "official") ++ serious.filter (·.name != "official")

private def heroSection (payload : Payload) : Block Page :=
  let tiles := #[
    statTile (toString payload.checkers.size) statCheckers,
    statTile (toString payload.tests.size) statTests
  ]
  sectionBlock "hero" #[
    blockHtml {{
      <div class="hero-main">
        <div class="hero-kicker">{{textHtml heroKicker}}</div>
        <h1 class="hero-title">{{textHtml heroTitle}}</h1>
        <p class="hero-copy">{{textHtml heroCopy}}</p>
        <div class="stat-strip">{{tiles}}</div>
      </div>
      <img class="hero-banner" src="static/kernel-arena-banner.jpg" alt={{siteTitle}}/>
    }}
  ]

private def mathlibTimeCell (stats : CheckerStats) (rate : Nat) : Html :=
  match stats.mathlib.bind (virtualSeconds · rate) with
  | some seconds =>
    let hint := match stats.mathlib with
      | some metrics => Arena.formatInstructions metrics.instructions.toFloat ++ " instructions"
      | none => ""
    {{ <td class="numeric" title={{hint}}>{{textHtml (Arena.formatDuration seconds)}}</td> }}
  | none => {{ <td class="numeric">{{textHtml missing}}</td> }}

private def mathlibMemoryCell (stats : CheckerStats) : Html :=
  match stats.mathlib.map (·.maxRss) with
  | some rss =>
    if rss > 0 then {{ <td class="numeric">{{textHtml (Arena.formatMemory rss.toFloat)}}</td> }}
    else {{ <td class="numeric">{{textHtml missing}}</td> }}
  | none => {{ <td class="numeric">{{textHtml missing}}</td> }}

private def checkerRow (checker : CheckerInfo) (rate : Nat) : Html :=
  let stats := checker.stats
  let accept := fractionTone stats.acceptCorrect stats.acceptTotal
  let reject := fractionTone stats.rejectCorrect stats.rejectTotal
  {{
    <tr>
      <td class="name"><a href={{checkerHref checker.name}}>{{textHtml checker.name}}</a></td>
      <td class="version">{{textHtml checker.version}}</td>
      <td class={{accept.className}}>
        {{textHtml (fraction stats.acceptCorrect stats.acceptTotal)}}
      </td>
      <td class={{reject.className}}>
        {{textHtml (fraction stats.rejectCorrect stats.rejectTotal)}}
      </td>
      <td class="cell">{{textHtml (toString stats.declined)}}</td>
      {{mathlibTimeCell stats rate}}
      {{mathlibMemoryCell stats}}
    </tr>
  }}

private def checkersTable (payload : Payload) : Block Page :=
  if payload.checkers.isEmpty then
    para #[text noCheckers]
  else
    blockHtml <| scrollTable {{
      <table class="arena-table">
        <thead>
          <tr>
            <th>{{textHtml columnChecker}}</th>
            <th>{{textHtml columnVersion}}</th>
            <th class="center" title={{acceptedColumnTitle}}>{{textHtml acceptedGlyph}}</th>
            <th class="center" title={{rejectedColumnTitle}}>{{textHtml rejectedGlyph}}</th>
            <th class="center" title={{declinedColumnTitle}}>{{textHtml declinedGlyph}}</th>
            <th class="numeric" title={{timeColumnTitle}}>{{textHtml timeGlyph}}</th>
            <th class="numeric" title={{memoryColumnTitle}}>{{textHtml memoryGlyph}}</th>
          </tr>
        </thead>
        <tbody>
          {{payload.checkers.map (checkerRow · payload.instructionsPerSecond)}}
        </tbody>
      </table>
    }}

private def officialBaseline (index : ResultIndex) (test : TestInfo) (rate : Nat) :
    Option Float := do
  let official ← findResult index "official" test.name
  guard (official.status == .accepted)
  let seconds ← virtualSeconds official.metrics rate
  guard (seconds ≥ timeComparisonFloor)
  return seconds

private def matrixCell (index : ResultIndex) (checker : CheckerInfo) (test : TestInfo)
    (rate : Nat) : Html :=
  match findResult index checker.name test.name with
  | none => emptyCell
  | some result =>
    let tone := toneOf result.status test.expectation
    let href := resultHref checker.name test.name
    let body :=
      if !perfComparable test result.status then
        textHtml (statusGlyph result.status)
      else if checker.name == "official" then
        match officialBaseline index test rate with
        | some seconds => textHtml (Arena.formatDuration seconds)
        | none => textHtml (statusGlyph result.status)
      else
        match officialBaseline index test rate, virtualSeconds result.metrics rate with
        | some baseline, some seconds => textHtml (formatPercent (percentChange baseline seconds))
        | _, _ => textHtml (statusGlyph result.status)
    statusCell tone href body

private def testRow (index : ResultIndex) (checkers : Array CheckerInfo) (test : TestInfo)
    (rate : Nat) : Html :=
  {{
    <tr>
      <td class="name"><a href={{testHref test.name}}>{{textHtml test.name}}</a></td>
      {{checkers.map (matrixCell index · test rate)}}
      <td class="numeric">{{textHtml (Arena.formatMemory test.size.toFloat)}}</td>
    </tr>
  }}

private def matrixHeader (checker : CheckerInfo) : Html :=
  {{
    <th class="rotated">
      <a href={{checkerHref checker.name}}>{{textHtml checker.name}}</a>
    </th>
  }}

private def testsTable (payload : Payload) : Block Page :=
  if payload.tests.isEmpty then
    para #[text noTests]
  else
    let index := indexOf payload
    let checkers := seriousCheckers payload
    blockHtml <| scrollTable {{
      <table class="arena-table matrix">
        <thead>
          <tr>
            <th>{{textHtml columnName}}</th>
            {{checkers.map matrixHeader}}
            <th class="numeric">{{textHtml columnSize}}</th>
          </tr>
        </thead>
        <tbody>
          {{payload.tests.map (testRow index checkers · payload.instructionsPerSecond)}}
        </tbody>
      </table>
    }}

private def measurementsBlock (payload : Payload) : Block Page :=
  let rate := Arena.formatUnitless payload.instructionsPerSecond.toFloat
  para #[text measurementsPrefix, code "mathlib", text (measurementsMiddle rate)]

private def tarballBlock (payload : Payload) : Block Page :=
  let tarball := payload.tarball
  para #[
    text tarballPrefix,
    .link #[code tarball.name] tarball.name,
    text (tarballSuffix tarball.goodCount tarball.badCount
      (Arena.formatMemory tarball.size.toFloat)),
    code "good/",
    text tarballBetweenDirs,
    code "bad/",
    text tarballTail
  ]

structure Prose where
  siteIntro : Array (Block Page)
  scoring : Array (Block Page)
  declining : Array (Block Page)
  interface : Array (Block Page)
  tutorial : Array (Block Page)
  extending : Array (Block Page)

def blocks (payload : Payload) (prose : Prose) : Array (Block Page) :=
  #[
    heroSection payload,
    sectionBlock "panel prose" prose.siteIntro,
    sectionBlock "panel" #[
      heading checkersHeading,
      checkersTable payload
    ],
    sectionBlock "panel" #[
      heading testsHeading,
      testsTable payload
    ],
    sectionBlock "panel prose" (
      #[heading detailsHeading]
      ++ prose.scoring
      ++ #[measurementsBlock payload]
      ++ prose.declining
      ++ prose.interface
      ++ #[tarballBlock payload]
      ++ prose.tutorial
      ++ prose.extending
    )
  ]

scoped syntax "arena_payload%" : term

elab_rules : term
  | `(arena_payload%) => do
    let payload ← loadPayload
    let payloadStx : TSyntax `term := quote payload.withoutProcessOutput
    let expected ← Lean.Elab.Term.elabTerm (← `(Payload)) none
    Lean.Elab.Term.elabTerm payloadStx (some expected)

end ArenaSite.Pages.Front
