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

private def checkersTable (payload : Payload) : Block Page :=
  if payload.checkers.isEmpty then
    para #[text noCheckers]
  else
    let rate := payload.instructionsPerSecond
    renderTable {
      nameLabel := columnChecker
      name := (·.name)
      href := checkerHref
      columns := #[
        column columnVersion fun checker =>
          {{ <td class="version">{{textHtml checker.version}}</td> }},
        column completenessColumn (align := .center) (title := acceptedColumnTitle)
          fun checker => fractionCell .incomplete checker.stats.acceptCorrect
            checker.stats.acceptTotal,
        column soundnessColumn (align := .center) (title := rejectedColumnTitle)
          fun checker => fractionCell .unsound checker.stats.rejectCorrect
            checker.stats.rejectTotal,
        column declinedColumn (align := .center) (title := declinedColumnTitle)
          fun checker => {{ <td class="cell">{{textHtml (toString checker.stats.declined)}}</td> }},
        column timeColumn (align := .numeric) (title := timeColumnTitle)
          fun checker => durationCell checker.stats.benchmark rate missing,
        column memoryColumn (align := .numeric) (title := memoryColumnTitle)
          fun checker => memoryCell checker.stats.benchmark missing
      ]
    } payload.checkers

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

private def sizeColumn : Column TestInfo :=
  column columnSize (align := .numeric)
    (groupCell := fun tests => sizeCell (tests.foldl (init := 0) fun n test => n + test.size))
    fun test => sizeCell test.size

private def checkerColumn (index : ResultIndex) (payload : Payload) (checker : CheckerInfo) :
    Column TestInfo :=
  let rate := payload.instructionsPerSecond
  {
    header := {{ <th class="rotated">
                   <a href={{checkerHref checker.name}}>{{textHtml checker.name}}</a>
                 </th> }}
    cell := fun test =>
      let baseline := baselineSeconds (findResult index payload.baselineChecker test.name) rate
      matrixCell index baseline (checker.name == payload.baselineChecker) checker test rate
    groupCell := fun tests =>
      tallyCell <| tests.filterMap fun test =>
        (findResult index checker.name test.name).map fun result =>
          ({ status := result.status, expectation := test.expectation } : Verdict)
  }

private def testsTable (payload : Payload) : Block Page :=
  if payload.tests.isEmpty then
    para #[text noTests]
  else
    let index := indexOf payload
    renderTable {
      extraClasses := " matrix"
      nameLabel := columnName
      name := (·.name)
      href := testHref
      columns := #[sizeColumn] ++ (seriousCheckers payload).map (checkerColumn index payload)
    } payload.tests

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
