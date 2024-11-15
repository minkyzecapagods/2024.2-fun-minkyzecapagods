module ExHyper where

import Prelude hiding ( exp )

-- Nat datatype --------------------------------

data Nat = O | S Nat
     deriving (Eq, Show)

instance (Num Nat) where
    (+) = add
    (*) = mul
    abs = id
    fromInteger 0 = O
    fromInteger n
      | n > 0     = S $ fromInteger (n-1)
      | otherwise = O
    signum O = O
    signum n = S O
    negate n = O

instance (Ord Nat) where
    O     <= m     = True
    (S n) <= O     = False
    (S n) <= (S m) = n <= m


toInt :: Nat -> Int
toInt O = 0
toInt (S n) = 1 + (toInt n)
------------------------------------------------

-- substitute 'undefined' by the correct number
-- to define each of those functions:

add :: Nat -> Nat -> Nat
add = hyper 1

mul :: Nat -> Nat -> Nat
mul = hyper 2

exp :: Nat -> Nat -> Nat
exp = hyper 3

-- hyper n should return the n'th operation in the sequence:
-- (..?..), add, mul, exp, ...?

{-- fold :: Num a => a -> (a -> a -> a) -> [a] -> a
fold i _ [] = i
fold i b (n : ns) = b n (fold i b ns)

repetir :: Nat -> Nat -> (Nat -> Nat -> Nat) -> Nat
repetir O _ _ = O
repetir (S n) m op = op m (repetir n m op)

prd :: Nat -> Nat
prd (S n) = n
prd _ = O

--}

hyper ::  Nat -> (Nat -> Nat -> Nat)
hyper O = \x -> \y -> S y
hyper (S n) = \x -> \y -> case y of
                            (S y') -> hyper n x (hyper (S n) x y')
                            _ -> case n of
                                  O -> x
                                  (S O) -> O
                                  _ -> (S O)
