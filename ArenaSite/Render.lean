import VersoBlog
import Arena.Util
import ArenaSite.Data
import ArenaSite.Copy

open Verso Doc Verso.Genre.Blog
open Verso.Output Html

namespace ArenaSite.Render

open ArenaSite.Copy

def textHtml (s : String) : Html := Html.text true s

def text (s : String) : Inline Page := .text s

def code (s : String) : Inline Page := .code s

def para (contents : Array (Inline Page)) : Block Page := .para contents

def blockHtml (html : Html) : Block Page := .other (BlockExt.blob html) #[]

def sectionBlock (classes : String) (contents : Array (Block Page)) : Block Page :=
  .other (BlockExt.htmlWrapper "section" #[("class", classes)]) contents

def pagePart (title : String) (content : Array (Block Page)) : Part Page :=
  .mk #[text title] title none content #[]

inductive Tone
  | good
  | unsound
  | incomplete
  | warn
  | neutral

def Tone.className : Tone → String
  | .good => "cell cell-good"
  | .unsound => "cell cell-unsound"
  | .incomplete => "cell cell-incomplete"
  | .warn => "cell cell-warn"
  | .neutral => "cell"

def toneOf (status : Status) (expectation : Option Expectation) : Tone :=
  if status.matches expectation then .good
  else if status.isInconclusive then .warn
  else if expectation == some .reject then .unsound
  else .incomplete

def statusGlyph : Status → String
  | .accepted => acceptedGlyph
  | .rejected => rejectedGlyph
  | .declined => declinedGlyph
  | .error => erroredGlyph

def expectationGlyph : Expectation → String
  | .accept => acceptedGlyph
  | .reject => rejectedGlyph

def virtualSeconds (metrics : Metrics) (rate : Nat) : Option Float :=
  if metrics.instructions > 0 && rate > 0 then
    some (metrics.instructions.toFloat / rate.toFloat)
  else if metrics.cpuNanos > 0 then some metrics.cpuSeconds
  else none

def percentChange (baseline current : Float) : Int :=
  ((current - baseline) / baseline * 100).round.toInt64.toInt

def formatPercent (percent : Int) : String :=
  if percent > 0 then s!"+{percent}%" else s!"{percent}%"

def timeComparisonFloor : Float := 0.05

def memoryComparisonFloor : Nat := 200 * 1024 * 1024

def perfComparable (test : TestInfo) (status : Status) : Bool :=
  test.comparePerf && test.expectation == some .accept && status == .accepted

def baselineSeconds (baseline : Option ResultInfo) (rate : Nat) : Option Float := do
  let baseline ← baseline
  guard (baseline.status == .accepted)
  let seconds ← virtualSeconds baseline.metrics rate
  guard (seconds ≥ timeComparisonFloor)
  return seconds

def timeDelta (test : TestInfo) (result : ResultInfo) (baseline : Option ResultInfo)
    (rate : Nat) : Option Int := do
  guard (perfComparable test result.status)
  let reference ← baselineSeconds baseline rate
  let seconds ← virtualSeconds result.metrics rate
  return percentChange reference seconds

def memoryDelta (test : TestInfo) (result : ResultInfo) (baseline : Option ResultInfo) :
    Option Int := do
  guard (perfComparable test result.status)
  let baseline ← baseline
  guard (baseline.status == .accepted)
  guard (baseline.metrics.maxRss ≥ memoryComparisonFloor)
  guard (result.metrics.maxRss > 0)
  return percentChange baseline.metrics.maxRss.toFloat result.metrics.maxRss.toFloat

def anchorId (test : String) : String := "test-" ++ test

def checkerHref (name : String) : String := s!"checker/{name}/"

def testHref (name : String) : String := s!"test/{name}/"

def resultHref (checker test : String) : String :=
  s!"{checkerHref checker}#{anchorId test}"

def statusCell (tone : Tone) (href : String) (body : Html) : Html :=
  {{ <td class={{tone.className}}><a class="cell-link" href={{href}}>{{body}}</a></td> }}

def emptyCell : Html := {{ <td class="cell">{{textHtml missing}}</td> }}

def numericCell (sortKey : Float) (rendered : String) (title : Option String := none) : Html :=
  Html.tag "td"
    (#[("class", "numeric"), ("data-sort", toString sortKey)]
      ++ (title.map (("title", ·))).toArray)
    (textHtml rendered)

def durationCell (metrics : Option Metrics) (rate : Nat) (placeholder : String) : Html :=
  match metrics.bind (virtualSeconds · rate) with
  | some seconds =>
    let instructions := (metrics.map (·.instructions)).getD 0
    let hint := Arena.formatUnitless instructions.toFloat ++ " instructions"
    numericCell seconds (Arena.formatDuration seconds) (some hint)
  | none => {{ <td class="numeric">{{textHtml placeholder}}</td> }}

def memoryCell (metrics : Option Metrics) (placeholder : String) : Html :=
  match metrics.map (·.maxRss) |>.filter (· > 0) with
  | some rss => numericCell rss.toFloat (Arena.formatMemory rss.toFloat)
  | none => {{ <td class="numeric">{{textHtml placeholder}}</td> }}

def sizeCell (bytes : Nat) : Html :=
  numericCell bytes.toFloat (Arena.formatMemory bytes.toFloat)

def deltaCell (delta : Option Int) : Html :=
  match delta with
  | some percent => {{ <td class="delta">{{textHtml s!"({formatPercent percent})"}}</td> }}
  | none => {{ <td class="delta"></td> }}

def linkGroup (declarationUrl sourceUrl : Option String) : Html :=
  let links := #[
    declarationUrl.map fun url => {{ <a href={{url}}>{{textHtml declarationLink}}</a> }},
    sourceUrl.map fun url => {{ <a href={{url}}>{{textHtml sourceLink}}</a> }}
  ].filterMap id
  if links.isEmpty then Html.empty else {{ <span class="link-group">{{links}}</span> }}

def metaItem (label value : String) : Html :=
  {{ <span class="meta-item"><b>{{textHtml label}}</b>{{textHtml (": " ++ value)}}</span> }}

def metaRow (items : Array Html) (links : Html) : Html :=
  {{ <p class="meta-row">{{items}}{{links}}</p> }}

def statTile (value label : String) : Html :=
  {{ <div class="stat-tile"><span class="stat-value">{{textHtml value}}</span>
       <label>{{textHtml label}}</label></div> }}

def scoreCard (heading value : String) : Html :=
  {{ <div class="score-card"><header>{{textHtml heading}}</header>
       <p class="score-value">{{textHtml value}}</p></div> }}

def fraction (numerator denominator : Nat) : String :=
  s!"{numerator}/{denominator}"

def fractionTone (shortfall : Tone) (numerator denominator : Nat) : Tone :=
  if denominator == 0 then .neutral
  else if numerator == denominator then .good
  else shortfall

def fractionCell (shortfall : Tone) (numerator denominator : Nat) : Html :=
  let ratio := if denominator == 0 then 0.0 else numerator.toFloat / denominator.toFloat
  {{ <td class={{(fractionTone shortfall numerator denominator).className}}
        data-sort={{toString ratio}}>{{textHtml (fraction numerator denominator)}}</td> }}

def pathGroup (name : String) : Option String :=
  match name.splitOn "/" with
  | head :: _ :: _ => some head
  | _ => none

def leafLabel (group name : String) : String :=
  (name.drop (group.length + 1)).toString

def groupPartition (key : α → String) (items : Array α) :
    Array α × Array (String × Array α) :=
  let names := items.foldl (init := #[]) fun order item =>
    match pathGroup (key item) with
    | some g => if order.contains g then order else order.push g
    | none => order
  let ungrouped := items.filter fun item => (pathGroup (key item)).isNone
  let grouped := names.map fun g =>
    (g, items.filter fun item => pathGroup (key item) == some g)
  (ungrouped, grouped)

structure Verdict where
  status : Status
  expectation : Option Expectation

def tally (verdicts : Array Verdict) : Nat × Nat × Bool :=
  verdicts.foldl (init := (0, 0, false)) fun (correct, total, unsound) v =>
    if v.status.isInconclusive then (correct, total, unsound)
    else
      let ok := v.status.matches v.expectation
      (correct + (if ok then 1 else 0), total + 1,
       unsound || (!ok && v.expectation == some .reject))

def tallyCell (verdicts : Array Verdict) : Html :=
  let (correct, total, unsound) := tally verdicts
  fractionCell (if unsound then .unsound else .incomplete) correct total

def disclosureSvg : Html := {{
  <svg class="disclosure" viewBox="0 0 16 16" aria-hidden="true">
    <path d="M5.5 3 L10.5 8 L5.5 13" fill="none" stroke="currentColor"
          stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
  </svg>
}}

inductive Align
  | left
  | center
  | numeric
  | delta

def Align.className : Align → String
  | .left => ""
  | .center => "center"
  | .numeric => "numeric"
  | .delta => "delta"

private def classAttr (classes : Array String) : Array (String × String) :=
  let joined := String.intercalate " " (classes.filter (!·.isEmpty)).toList
  if joined.isEmpty then #[] else #[("class", joined)]

def blankCell (align : Align) : Html :=
  Html.tag "td" (classAttr #[align.className]) Html.empty

structure Column (α : Type) where
  header : Html
  cell : α → Html
  groupCell : Array α → Html

def headerCell (label : String) (align : Align) (sortable : Bool) (title : String) : Html :=
  Html.tag "th"
    (classAttr #[align.className, if sortable then "sortable" else ""]
      ++ (if title.isEmpty then #[] else #[("title", title)]))
    (textHtml label)

def column (label : String) (cell : α → Html) (align : Align := .left)
    (sortable : Bool := true) (title : String := "")
    (groupCell : Array α → Html := fun _ => blankCell align) : Column α :=
  { header := headerCell label align sortable title, cell, groupCell }

structure Table (α : Type) where
  extraClasses : String := ""
  nameLabel : String
  name : α → String
  href : String → String
  groupHref : String → String := testHref
  columns : Array (Column α)

private def nameCell (href label : String) : Html :=
  {{ <td class="name"><a href={{href}}>{{textHtml label}}</a></td> }}

private def groupNameCell (href group : String) (count : Nat) : Html :=
  {{
    <td class="name" data-sort={{group}}>
      {{disclosureSvg}}<a href={{href}}>{{textHtml group}}</a>{{textHtml s!" ({count})"}}
    </td>
  }}

def renderTable (t : Table α) (items : Array α) : Block Page :=
  let (ungrouped, groups) := groupPartition t.name items
  let leaf (rowClass label : String) (item : α) : Html :=
    {{ <tr class={{rowClass}}>
         {{nameCell (t.href (t.name item)) label}}{{t.columns.map (·.cell item)}}
       </tr> }}
  let summary (group : String) (members : Array α) : Html :=
    {{ <tr class="group-row">
         {{groupNameCell (t.groupHref group) group members.size}}
         {{t.columns.map (·.groupCell members)}}
       </tr> }}
  let bodies : Array Html :=
    (ungrouped.map fun item => {{ <tbody>{{leaf "" (t.name item) item}}</tbody> }})
    ++ groups.map fun (group, members) =>
      {{ <tbody class="group">
           {{summary group members}}
           {{members.map fun item => leaf "group-child" (leafLabel group (t.name item)) item}}
         </tbody> }}
  blockHtml {{
    <div class="table-scroll">
      <table class={{"arena-table" ++ t.extraClasses}}>
        <thead><tr>
          {{headerCell t.nameLabel .left true ""}}{{t.columns.map (·.header)}}
        </tr></thead>
        {{bodies}}
      </table>
    </div>
  }}

def perfColumns (test : α → TestInfo) (result : α → ResultInfo)
    (baseline : α → Option ResultInfo) (rate : Nat) : Array (Column α) :=
  #[column timeColumn (align := .numeric) fun row =>
      durationCell (some (result row).metrics) rate "",
    column "" (align := .delta) (sortable := false) fun row =>
      deltaCell (timeDelta (test row) (result row) (baseline row) rate),
    column memoryColumn (align := .numeric) fun row =>
      memoryCell (some (result row).metrics) "",
    column "" (align := .delta) (sortable := false) fun row =>
      deltaCell (memoryDelta (test row) (result row) (baseline row))]

def statusColumn (label : String) (test : α → TestInfo) (result : α → ResultInfo) : Column α :=
  column label (align := .center)
    (groupCell := fun rows => tallyCell <| rows.map fun row =>
      { status := (result row).status, expectation := (test row).expectation })
    fun row =>
      let status := (result row).status
      let expectation := (test row).expectation
      Html.tag "td"
        #[("class", (toneOf status expectation).className), ("title", toString status),
          ("data-sort", if status.matches expectation then "1" else "0")]
        (textHtml (statusGlyph status))

def breadcrumb (name : String) : Block Page :=
  blockHtml {{
    <h1 class="breadcrumb">
      <a href=".">{{textHtml breadcrumbRoot}}</a>{{textHtml breadcrumbSeparator}}{{textHtml name}}
    </h1>
  }}

def heading (title : String) : Block Page :=
  blockHtml {{ <h2>{{textHtml title}}</h2> }}

def preBlock (label content : String) : Array Html :=
  if content.isEmpty then #[]
  else #[{{ <h5>{{textHtml label}}</h5> }}, {{ <pre>{{textHtml content}}</pre> }}]

end ArenaSite.Render
