module Nat where
import Prelude ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

data Nat = O | S Nat
{--
 - data Nat where
 - O :: Nat
 - S :: Nat -> Nat
 --}

-- Print Nats
instance Show Nat where
  show O = "0"
  show (S n) = "S" ++ show n

-- Igualdade em Nats
instance Eq Nat where
  (==) O O = True
  (==) (S n) (S m) = n == m
  (==) _ _ = False

-- Ordem em Nats
instance Ord Nat where
  (<=) O m = True
  (<=) (S n) (S m) = n <= m
  (<=) _ _ = False
  (>=) n m = not(n <= m)
  (<) n m = not(n == m) && (n <= m)
  (>) n m = not(n == m) && (n >= m)

-- Transforma Integral em Nat
toNat :: Integral a => a -> Nat
toNat 0 = O
toNat n = S (toNat (n - 1))

-- Transforma Nat em Integral
fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n

-- Adicao de tulpa de Nats
add :: (Nat, Nat) -> Nat
add (n,O) = n
add (n, S m) = S(add (n, m))

-- Subtracao de tulpa de Nats
minus :: (Nat, Nat) -> Nat
minus (n, O) = n
minus (S n, S m) = minus (n, m)
minus (_, _) = O

-- Multiplicacao de tulpa de Nats
mult :: (Nat, Nat) -> Nat
mult (n, O) = O
mult (n, S m) = add (n, mult (n, m))

-- Exponencicao de tulpa de Nats
exp :: (Nat, Nat) -> Nat
exp (n, O) = S O
exp (n, S m) = mult (n, exp (n, m))

-- Dobro de um Nat
double :: Nat -> Nat
double O = O
double (S n) = S(S(double n))

-- Predecessor de um Nat
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- Fatorial de um Nat
fact :: Nat -> Nat
fact O = (S O)
fact (S n) = mult (S n, (fact n))

-- Nat da posicao de um Nat na sequencia de fibonacci
fib :: Nat -> Nat
fib (S (S n)) = add (S n, n)
fib n = n

-- Menor Nat de uma tulpa de Nats
menor :: (Nat, Nat) -> Nat
menor (S n, S m) = S (menor (n, m))
menor (_, _) = O

-- Maior Nat de uma tulpa de Nats
maior :: (Nat, Nat) -> Nat
maior (S n, S m) = S (maior (n, m))
maior (O, n) = n
maior (n, O) = n

-- Divisao de dois Nats, com quociente e resto
divisao :: (Nat, Nat) -> (Nat, Nat)
divisao (n, m) = (quoc (n, m), res (n, m))

-- Resto de uma divisao de dois Nats
res :: (Nat, Nat) -> Nat
res (n, O) = error "nzd"
res (n, m)
        | m <= n = res (minus (n, m), m)
        | otherwise = n

-- Quociente de uma divisao de dois Nats
quoc :: (Nat, Nat) -> Nat
quoc (_, O) = error "nzd"
quoc (n, m) = if m <= n
              then S(quoc (minus (n, m), m))
              else O

-- Maior divisor comum entre dois Nats
gcd :: (Nat, Nat) -> Nat 
gcd (n, O) = n 
gcd (O, m) = m 
gcd (n, m) = gcd (m, res (n, m))

-- Menor multiplo comum entre dois Nats
lcm :: (Nat, Nat) -> Nat
lcm (n, m) = quoc (mult (n, m), gcd (n, m))

