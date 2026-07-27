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

def link (label url : String) : Inline Page := .link #[.text label] url

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
  if status.satisfies expectation then .good
  else if status.isInconclusive then .warn
  else .bad

def statusGlyph : Status → String
  | .accepted => acceptedGlyph
  | .rejected => rejectedGlyph
  | .declined => declinedGlyph
  | .error => erroredGlyph

def statusLabel : Status → String
  | .accepted => acceptedLabel
  | .rejected => rejectedLabel
  | .declined => declinedLabel
  | .error => erroredLabel

def expectationGlyph : Expectation → String
  | .accept => acceptedGlyph
  | .reject => rejectedGlyph

def expectationLabel : Expectation → String
  | .accept => "accept"
  | .reject => "reject"

def virtualSeconds (metrics : Metrics) (rate : Nat) : Option Float :=
  if metrics.instructions > 0 && rate > 0 then
    some (Arena.instructionsToTime metrics.instructions rate.toFloat)
  else if metrics.cpuNanos > 0 then some metrics.cpuSeconds
  else none

def roundToInt (x : Float) : Int :=
  if x < 0 then -(((-x) + 0.5).floor.toUInt64.toNat : Int)
  else ((x + 0.5).floor.toUInt64.toNat : Int)

def percentChange (baseline current : Float) : Int :=
  roundToInt ((current - baseline) / baseline * 100)

def formatPercent (percent : Int) : String :=
  if percent > 0 then s!"+{percent}%" else s!"{percent}%"

def timeComparisonFloor : Float := 0.05

def memoryComparisonFloor : Nat := 200 * 1024 * 1024

def perfComparable (test : TestInfo) (status : Status) : Bool :=
  test.comparePerf && test.expectation == some .accept && status == .accepted

def anchorId (test : String) : String := "test-" ++ test

def checkerHref (name : String) : String := s!"checker/{name}/"

def testHref (name : String) : String := s!"test/{name}/"

def resultHref (checker test : String) : String :=
  s!"{checkerHref checker}#{anchorId test}"

private def cellLink (href : String) (body : Html) : Html :=
  {{ <a class="cell-link" href={{href}}>{{body}}</a> }}

def statusCell (tone : Tone) (href : String) (body : Html) : Html :=
  {{ <td class={{tone.className}}>{{cellLink href body}}</td> }}

def emptyCell : Html := {{ <td class="cell">{{textHtml missing}}</td> }}

private def linkList (declarationUrl sourceUrl : Option String) : Array Html :=
  #[declarationUrl.map fun url => {{ <a href={{url}}>{{textHtml declarationLink}}</a> }},
    sourceUrl.map fun url => {{ <a href={{url}}>{{textHtml sourceLink}}</a> }}].filterMap id

def linkGroup (declarationUrl sourceUrl : Option String) : Html :=
  let links := linkList declarationUrl sourceUrl
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

def breadcrumb (name : String) : Block Page :=
  blockHtml {{
    <h1 class="breadcrumb">
      <a href=".">{{textHtml breadcrumbRoot}}</a>{{textHtml breadcrumbSeparator}}{{textHtml name}}
    </h1>
  }}

def heading (title : String) : Block Page :=
  blockHtml {{ <h2>{{textHtml title}}</h2> }}

def scrollTable (contents : Html) : Html :=
  {{ <div class="table-scroll">{{contents}}</div> }}

def preBlock (label content : String) : Array Html :=
  if content.isEmpty then #[]
  else #[{{ <h5>{{textHtml label}}</h5> }}, {{ <pre>{{textHtml content}}</pre> }}]

end ArenaSite.Render
