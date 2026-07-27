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
  | bad
  | warn
  | neutral

def Tone.className : Tone → String
  | .good => "cell cell-good"
  | .bad => "cell cell-bad"
  | .warn => "cell cell-warn"
  | .neutral => "cell"

def toneOf (status : Status) (expectation : Option Expectation) : Tone :=
  if status.matches expectation then .good
  else if status.isInconclusive then .warn
  else .bad

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

def durationCell (metrics : Option Metrics) (rate : Nat) (placeholder : String) : Html :=
  match metrics.bind (virtualSeconds · rate) with
  | some seconds =>
    let instructions := (metrics.map (·.instructions)).getD 0
    let hint := Arena.formatUnitless instructions.toFloat ++ " instructions"
    {{ <td class="numeric" title={{hint}}>{{textHtml (Arena.formatDuration seconds)}}</td> }}
  | none => {{ <td class="numeric">{{textHtml placeholder}}</td> }}

def memoryCell (metrics : Option Metrics) (placeholder : String) : Html :=
  match metrics.map (·.maxRss) |>.filter (· > 0) with
  | some rss => {{ <td class="numeric">{{textHtml (Arena.formatMemory rss.toFloat)}}</td> }}
  | none => {{ <td class="numeric">{{textHtml placeholder}}</td> }}

def deltaCell (delta : Option Int) : Html :=
  match delta with
  | some percent => {{ <td class="delta">{{textHtml s!"({formatPercent percent})"}}</td> }}
  | none => {{ <td class="delta"></td> }}

def perfCells (test : TestInfo) (result : ResultInfo) (baseline : Option ResultInfo)
    (rate : Nat) : Array Html :=
  #[durationCell (some result.metrics) rate "",
    deltaCell (timeDelta test result baseline rate),
    memoryCell (some result.metrics) "",
    deltaCell (memoryDelta test result baseline)]

def perfHeaderCells : Array Html :=
  #[{{ <th class="numeric">{{textHtml timeGlyph}}</th> }}, {{ <th class="delta"></th> }},
    {{ <th class="numeric">{{textHtml memoryGlyph}}</th> }}, {{ <th class="delta"></th> }}]

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

def fractionTone (numerator denominator : Nat) : Tone :=
  if denominator == 0 then .neutral
  else if numerator == denominator then .good
  else .bad

def dataTable (extraClasses : String) (headers rows : Array Html) : Block Page :=
  blockHtml {{
    <div class="table-scroll">
      <table class={{"arena-table" ++ extraClasses}}>
        <thead><tr>{{headers}}</tr></thead>
        <tbody>{{rows}}</tbody>
      </table>
    </div>
  }}

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
