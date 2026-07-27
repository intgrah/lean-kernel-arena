namespace Arena

inductive Expectation
  | accept
  | reject
deriving DecidableEq, Repr, Inhabited

def Expectation.toString : Expectation → String
  | .accept => "accept"
  | .reject => "reject"

instance : ToString Expectation := ⟨Expectation.toString⟩

def Expectation.ofString? : String → Option Expectation
  | "accept" => some .accept
  | "reject" => some .reject
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
  | _, _ => false

def Status.isInconclusive : Status → Bool
  | .declined | .error => true
  | _ => false

inductive Correctness
  | correct
  | incorrect
  | declined
  | error
deriving DecidableEq, Repr, Inhabited

def Correctness.toString : Correctness → String
  | .correct => "correct"
  | .incorrect => "incorrect"
  | .declined => "declined"
  | .error => "error"

instance : ToString Correctness := ⟨Correctness.toString⟩

def Correctness.of (status : Status) (expectation : Option Expectation) : Correctness :=
  if status == .declined then .declined
  else if status == .error || expectation.isNone then .error
  else if status.matches expectation then .correct
  else .incorrect

def Correctness.all : List Correctness := [.correct, .incorrect, .declined, .error]

end Arena
