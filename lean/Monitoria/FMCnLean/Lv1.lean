namespace Lv1

-- Nao mexas neste inicio :P

inductive Nat where
  | O : Nat
  | S : Nat → Nat

open Nat

def fromNat : Nat -> String
  | O   => "0"
  | S O => "S 0"
  | S n => "S (" ++ fromNat n ++ ")"

instance : Repr Nat where
  reprPrec
    | n, _  => Std.Format.text (fromNat n)

---------------------------------------------Defs---------------------------------------------

-- A adição(+) eu vou dar de graça
-- Use esse presente para fazer mais funções!

def add : Nat → Nat → Nat
  | n, O   => n
  | n, S m => S (add n m)

-- Vamos conferir ..
#eval add (S O) O
#eval add (S (S O)) (S O)

                     /- Agora vamos definir algumas funções!
                        Presta atencao de colocar o tipo certo!
                          Obs: Se quiser escrever `→` é só digitar \to -/

-- Multiplicação (*)
def mul : Nat → Nat → Nat
  | _, O   => O
  | n, S m => add n (mul n m)

#eval mul (S (S O)) (O)
#eval mul (S O) (S (S (S O)))
#eval mul (S (S (S O))) (S (S O))

-- Exponenciação (^)
def exp : Nat → Nat → Nat
  | _, O => S O
  | n, S m => mul n (exp n m)

#eval exp (S (S (S O))) (S O)
#eval exp (S (S (S (S O)))) O
#eval exp (S (S (S O))) (S (S (S (S O))))
#eval exp (S O) (S (S (S (S O))))

-- Fibonacci
def fib : Nat → Nat
  | O => O
  | S O => S O
  | S (S n) => add (fib (S n)) (fib n)

#eval fib O
#eval fib (S O)
#eval fib (S (S O))
#eval fib (S (S (S O)))
#eval fib (S (S (S (S O))))
#eval fib (S (S (S (S (S O)))))

-- Factorial
def fact : Nat → Nat
  | O => S O
  | S n => mul (fact n) (S n)

#eval fact O
#eval fact (S O)
#eval fact (S (S O))
#eval fact (S (S (S O)))
#eval fact (S (S (S (S O))))
#eval fact (S (S (S (S (S O)))))

-- Min binario
def min : Nat → Nat → Nat
  | S n, S m => S (min n m)
  | _, _ => O

#eval min (S (S O)) O
#eval min (S O) (S (S O))

-- Max binario
def max : Nat → Nat → Nat
  | O, n => n
  | n, O => n
  | S n, S m => S (max n m)

#eval max (S (S O)) O
#eval max O (S (S O))
#eval max (S O) (S (S O))

def toRootNat : Nat → _root_.Nat
  | O => 0
  | S n => 1 + toRootNat n

#eval toRootNat O
#eval toRootNat (S O)

                      /- Pergunta qualquer duvida sobre as defs -/

-----------------------------------------Demons---------------------------------------------

-- Exemplo :
theorem add_zero :
  ∃(e : Nat), ∀(n : Nat), add n e = n :=
by
  exists O
  intro n
  rw [add]

theorem mul_one :
  ∃(e : Nat), ∀(n : Nat), mul n e = n :=
by
  exists (S O)
  intro n
  repeat rw[mul]
  rw[add]

theorem exp_one :
  ∃(e : Nat), ∀(n : Nat), exp n e = n :=
by
  exists (S O)
  intro n
  repeat rw[exp]
  repeat rw[mul]
  rw[add]

  theorem max_id_O :
  ∃(e : Nat), ∀(n : Nat), max n e = n ∧ max e n = n :=
by
  exists O
  intro n
  apply And.intro
  cases n with
    | O => rw[max]
    | S m => rw[max]; intro h; cases h
  rw[max]
