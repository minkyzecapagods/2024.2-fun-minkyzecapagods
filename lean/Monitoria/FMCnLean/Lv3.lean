
-- A Lv based on ListNat game, made by Isaac:
-- https://github.com/isaacmsl/ListNatGame/blob/main/ListNatGame.lean
---------------------------------------------------------------------

inductive ListNat where
  | nil
  | cons : Nat → ListNat → ListNat
  deriving Repr

-- Pt 1: Recognizing


open ListNat -- Easier to write ListNat things


namespace Lv3

#check nil -- With open command
#check ListNat.nil -- Without "open ListNat" command
#check cons 0 nil
#eval nil -- deriving Repr








-- Pt 2: Defining operations

def length : ListNat → Nat
  | nil => 0
  | cons _ l => length l + 1

#check length nil -- Judgement
#eval length nil -- Evaluation (?)
#eval length (cons 0 nil) -- Fix it

def greatest : ListNat → Nat
  | nil => 0
  | cons n l => if n > (greatest l) then n else (greatest l)

#eval greatest nil
#eval greatest (cons 4 (cons 20 (cons 30 nil)))
#eval greatest (cons 1 (cons 1 nil))

-- Assume ordered lists
-- First element greater than n
def upper_bound : Nat → ListNat → Nat
  | _, nil => 0
  | n, cons l ls => if n < l then l else upper_bound n ls

#eval upper_bound 0 nil
#eval upper_bound 3 (cons 1 (cons 3 (cons 4 (cons 8 nil))))
#eval upper_bound 5 (cons 1 (cons 3 (cons 4 (cons 8 nil))))

-- Assume ordered lists
-- First element not less than n
def lower_bound : Nat → ListNat → Nat
  | _, nil => 0
  | n, cons l ls => if n <= l then l else lower_bound n ls

#eval lower_bound 0 nil
#eval lower_bound 3 (cons 1 (cons 3 (cons 4 (cons 8 nil))))
#eval lower_bound 5 (cons 1 (cons 3 (cons 4 (cons 8 nil))))

def filter : (Nat → Bool) → ListNat → ListNat
  | _, nil => nil
  | p, cons l ls => if p l then cons l (filter p ls) else filter p ls

open Nat

def even : Nat → Bool
 | 0 => True
 | succ 0 => False
 | succ (succ n)=> even n

#eval filter even nil
#eval filter even (cons 0 (cons 2 (cons 3 nil)))

-- [1,2,3] → [2,4,6]
def doubleList : ListNat → ListNat
  | nil => nil
  | cons n ns => cons (n * 2) (doubleList ns)

def map : (Nat → Nat) → ListNat → ListNat
  | _, nil => nil
  | f, cons n ns => cons (f n) (map f ns)

#eval doubleList nil
#eval doubleList (cons 1 (cons 3 nil))

-- Use \. + space to write ·
#eval map (· * 2) (cons 1 (cons 3 nil))
#eval map (· + 3) (cons 1 nil)


def concat : ListNat → ListNat → ListNat
  | nil, ns => ns
  | cons l ls, ns => cons l (concat ls ns)

#eval concat nil nil
#eval concat (cons 1 nil) (cons 3 (cons 4 nil))
-- Yours!

-- Add an nat to the end
def append : Nat → ListNat → ListNat
  | n, cons l ls => cons l (append n ls)
  | n, _ => cons n nil

-- Define append in function of concat

def append_cat : Nat → ListNat → ListNat
  | n, ns => concat ns (cons n nil)

#eval append 0 nil
#eval append 3 (cons 0 nil)

-- esreveR...
def reverse : ListNat → ListNat
  | cons n ns => append n (reverse ns)
  | _ => nil

#eval reverse  (cons 0 (cons 2 (cons 3 nil)))

-- Pt 3: Theorems

variable (l : ListNat)

theorem same_functions :
  doubleList l = map (· * 2) l :=
by
  sorry

theorem reverse_nil : reverse nil = nil := rfl

theorem cool_theorem (x : Nat) :
  reverse (append x l) = cons x (reverse l) :=
by
  sorry

theorem reverse_reverse :
  reverse (reverse l) = l :=
by
  sorry








-- Pt 4: Your own theorems

-- Sugestions:
-- Can you think cool theorems about lower_bound and upper_bound?
-- How about append_cat?
-- Buy me a KitKat from myself

-- By Isaac Lourenço 2023, IMD - UFRN, Brazil.
