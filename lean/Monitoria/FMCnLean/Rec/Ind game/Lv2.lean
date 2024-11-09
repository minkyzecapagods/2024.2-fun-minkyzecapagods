import FMCnLean.Lv1
-- Se teu outro arquivo compilou,
-- talvez tu precise usar Ctrl + Shift + P : Restart File

open Lv1
open Lv1.Nat

namespace Lv2

def Nat := Lv1.Nat

---------------------------------------------Defs---------------------------------------------

inductive Bool where
  | True : Bool
  | False : Bool
deriving Repr

open Bool

-- Logical and         (∧)
def land : Bool → Bool → Bool
  | Bool.True, Bool.True => True
  | _, _ => False

#eval land True True
#eval land True False
#eval land False True
#eval land False False

-- Logical or          (∨)
def lor : Bool → Bool → Bool
  | Bool.False, Bool.False => False
  | _, _ => True

#eval lor True True
#eval lor True False
#eval lor False True
#eval lor False False

-- Logical not         (¬)
def lneg : Bool → Bool
  | Bool.True => False
  | _ => True

#eval lneg True
#eval lneg False

-- Logical implies     (→)
def limplies : Bool → Bool → Bool
  | p, q => lor (lneg p) q

/- Sem usar lneg
def limplies: Bool → Bool → Bool
  | Bool.False, _ => True
  | p, q => land p q
-/

#eval limplies False True
#eval limplies False False
#eval limplies True True
#eval limplies True False

-- Logical impliedBy   (←)
def limplied : Bool → Bool → Bool
  | p, q => lneg (limplies p q)

#eval limplied False True
#eval limplied True False

-- Logical NAND
def lnand : Bool → Bool → Bool
  | p, q=> lneg (land p q)

#eval lnand True True
#eval lnand False False
#eval lnand True False

-- Logical NOR
def lnor : Bool → Bool → Bool
  | p, q => lneg (lor p q)

#eval lnor True False
#eval lnor True True

-- Logical Exclusive-or
def lxou : Bool → Bool → Bool
  | p, q => lor (lnor p q) (lnand p q)

#eval lxou True  True
#eval lxou False False
#eval lxou True  False

--  Par
def isEven : Nat → Bool
  | O => True
  | S (S n) => isEven n
  | _ => False

#eval isEven O
#eval isEven (S O)
#eval isEven (S (S O))

-- Impar
def isOdd : Nat → Bool
  | n => lneg (isEven n)

#eval isOdd (S (S O))
#eval isOdd (S (S (S O)))

-- (<)
def isLt : Nat → Nat → Bool
  | O, O => False
  | O, _=> True
  | _, O => False
  | S n, S m => isLt n m

#eval isLt (S O) (S (S O))
#eval isLt (S (S (S O))) (S O)

-- (==)
def isEq : Nat → Nat → Bool
  | O, O => True
  | S n, S m => isEq n m
  | _, _ => False

#eval isEq O (S (S O))
#eval isEq (S O) (S O)

-- (>)
def isGt : Nat → Nat → Bool
  | O, O => False
  | n, m => lneg (isLt n m)

def bool_to_prop : Bool → Prop
  | Bool.True  => true
  | _ => false

#eval isGt O (S (S O))
#eval isGt (S (S O)) (S O)

-----------------------------------------Demons---------------------------------------------


theorem land_symm:
  ∃(e : Bool), ∀(b : Bool), (land b e = b) ∧ (land e b = b) :=
by
  exists True
  intro b
  apply And.intro
  repeat cases b with
  | True => rw[land]
  | False => simp[land]


theorem lor_symm:
  ∃(e : Bool), ∀(b : Bool), (lor b e = b) ∧ (lor e b = b) :=
by
  exists False
  intro b
  apply And.intro
  repeat cases b with
  | True => simp[lor]
  | False => rw[lor]


theorem neg_neg:
   ∀(b : Bool), lneg (lneg b) = b :=
by
  intro b
  cases b with
  | True => repeat simp[lneg]
  | False => repeat simp[lneg]

theorem ne_less_zero:
  ∀(n : Nat), isLt O n = Bool.True ∨ (bool_to_prop (isEq O n)) :=
by
  intro n
  cases n with
  | O => right; simp[isEq]; rw[bool_to_prop]
  | S n' => left; simp [isLt]


/-
theorem even_ne_odd:
∀(n : Nat), isEven n = lneg (isOdd n) :=
by
  intro n
  induction n
-/
