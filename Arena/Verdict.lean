import Lean.Data.Json

open Lean (Json ToJson FromJson toJson)

namespace Arena

inductive Expectation
  | accept
  | reject
  | either
deriving DecidableEq, Repr, Inhabited

def Expectation.toString : Expectation → String
  | .accept => "accept"
  | .reject => "reject"
  | .either => "either"

instance : ToString Expectation := ⟨Expectation.toString⟩

def Expectation.ofString? : String → Option Expectation
  | "accept" => some .accept
  | "reject" => some .reject
  | "either" => some .either
  | _ => none

inductive Status
  | accepted
  | rejected
  | declined
  | error
deriving DecidableEq, Repr, Inhabited

def Status.ofExitCode : Int → Status
  | 0 => .accepted
  | 1 => .rejected
  | 2 => .declined
  | _ => .error

def Status.toString : Status → String
  | .accepted => "accepted"
  | .rejected => "rejected"
  | .declined => "declined"
  | .error => "error"

instance : ToString Status := ⟨Status.toString⟩

def Status.matches : Status → Option Expectation → Bool
  | .accepted, some .accept => true
  | .rejected, some .reject => true
  | .accepted, some .either => true
  | .rejected, some .either => true
  | _, _ => false

def Status.isInconclusive : Status → Bool
  | .declined | .error => true
  | _ => false

inductive Attempt
  | ran (exitCode : Int) (stdout stderr : String)
  | declined (reason : String)
  | skipped (reason : String)
deriving DecidableEq, Repr, Inhabited

def Attempt.status : Attempt → Status
  | .ran exitCode _ _ => Status.ofExitCode exitCode
  | .declined _ => .declined
  | .skipped _ => .error

def Attempt.withoutProcessOutput : Attempt → Attempt
  | .ran exitCode _ _ => .ran exitCode "" ""
  | attempt => attempt

instance : ToJson Attempt where
  toJson
    | .ran exitCode stdout stderr => Json.mkObj [
        ("exit_code", toJson exitCode),
        ("stdout", toJson stdout),
        ("stderr", toJson stderr)
      ]
    | .declined reason => Json.mkObj [("declined", toJson reason)]
    | .skipped reason => Json.mkObj [("skipped", toJson reason)]

instance : FromJson Attempt where
  fromJson? json := do
    match json.getObjValAs? String "skipped", json.getObjValAs? String "declined" with
    | .ok reason, _ => return .skipped reason
    | _, .ok reason => return .declined reason
    | _, _ =>
      return .ran (← json.getObjValAs? Int "exit_code")
        (← json.getObjValAs? String "stdout") (← json.getObjValAs? String "stderr")

inductive Correctness
  | correct
  | incorrect
  | either
  | declined
  | error
deriving DecidableEq, Repr, Inhabited

def Correctness.toString : Correctness → String
  | .correct => "correct"
  | .incorrect => "incorrect"
  | .either => "either"
  | .declined => "declined"
  | .error => "error"

instance : ToString Correctness := ⟨Correctness.toString⟩

def Correctness.of : Status → Option Expectation → Correctness
  | .declined, _ => .declined
  | .error, _ => .error
  | _, none => .error
  | _, some .either => .either
  | .accepted, some .accept => .correct
  | .rejected, some .reject => .correct
  | .accepted, some .reject => .incorrect
  | .rejected, some .accept => .incorrect

def Correctness.all : List Correctness := [.correct, .incorrect, .either, .declined, .error]

end Arena
