import ArenaSite.Render

open Lean
open Lean.Elab Term
open Verso Doc Verso.Genre.Blog
open Verso.Output Html

namespace ArenaSite.Pages.Front

open ArenaSite.Render
open ArenaSite.Copy

private def seriousCheckers (payload : Payload) : Array CheckerInfo :=
  let serious := payload.checkers.filter (·.serious)
  let isBaseline (checker : CheckerInfo) := checker.name == payload.baselineChecker
  serious.filter isBaseline ++ serious.filter (!isBaseline ·)

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
      {{durationCell stats.benchmark rate missing}}
      {{memoryCell stats.benchmark missing}}
    </tr>
  }}

private def checkersTable (payload : Payload) : Block Page :=
  if payload.checkers.isEmpty then
    para #[text noCheckers]
  else
    dataTable "" #[
      {{ <th>{{textHtml columnChecker}}</th> }},
      {{ <th>{{textHtml columnVersion}}</th> }},
      {{ <th class="center" title={{acceptedColumnTitle}}>{{textHtml acceptedGlyph}}</th> }},
      {{ <th class="center" title={{rejectedColumnTitle}}>{{textHtml rejectedGlyph}}</th> }},
      {{ <th class="center" title={{declinedColumnTitle}}>{{textHtml declinedGlyph}}</th> }},
      {{ <th class="numeric" title={{timeColumnTitle}}>{{textHtml timeGlyph}}</th> }},
      {{ <th class="numeric" title={{memoryColumnTitle}}>{{textHtml memoryGlyph}}</th> }}
    ] (payload.checkers.map (checkerRow · payload.instructionsPerSecond))

private def matrixCell (index : ResultIndex) (baseline : Option Float) (isBaseline : Bool)
    (checker : CheckerInfo) (test : TestInfo) (rate : Nat) : Html :=
  match findResult index checker.name test.name with
  | none => emptyCell
  | some result =>
    let comparison : Option Html := do
      guard (perfComparable test result.status)
      let reference ← baseline
      if isBaseline then
        return textHtml (Arena.formatDuration reference)
      let seconds ← virtualSeconds result.metrics rate
      return textHtml (formatPercent (percentChange reference seconds))
    statusCell (toneOf result.status test.expectation) (resultHref checker.name test.name)
      (comparison.getD (textHtml (statusGlyph result.status)))

private def testRow (index : ResultIndex) (checkers : Array CheckerInfo) (payload : Payload)
    (test : TestInfo) : Html :=
  let rate := payload.instructionsPerSecond
  let baseline := baselineSeconds (findResult index payload.baselineChecker test.name) rate
  {{
    <tr>
      <td class="name"><a href={{testHref test.name}}>{{textHtml test.name}}</a></td>
      {{checkers.map fun checker =>
        matrixCell index baseline (checker.name == payload.baselineChecker) checker test rate}}
      <td class="numeric">{{textHtml (Arena.formatMemory test.size.toFloat)}}</td>
    </tr>
  }}

private def testsTable (payload : Payload) : Block Page :=
  if payload.tests.isEmpty then
    para #[text noTests]
  else
    let index := indexOf payload
    let checkers := seriousCheckers payload
    let headers :=
      #[{{ <th>{{textHtml columnName}}</th> }}]
      ++ checkers.map (fun checker =>
        {{ <th class="rotated">
             <a href={{checkerHref checker.name}}>{{textHtml checker.name}}</a>
           </th> }})
      ++ #[{{ <th class="numeric">{{textHtml columnSize}}</th> }}]
    dataTable " matrix" headers (payload.tests.map (testRow index checkers payload))

private def measurementsBlock (payload : Payload) : Block Page :=
  let rate := Arena.formatUnitless payload.instructionsPerSecond.toFloat
  para #[text measurementsPrefix, code payload.benchmarkTest, text (measurementsMiddle rate)]

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
    sectionBlock "panel" #[heading checkersHeading, checkersTable payload],
    sectionBlock "panel" #[heading testsHeading, testsTable payload],
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
    let stx : TSyntax `term := quote payload.withoutProcessOutput
    elabTerm stx (some (mkConst ``Payload))

end ArenaSite.Pages.Front
