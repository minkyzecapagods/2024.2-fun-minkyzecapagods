{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
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

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) O O = True
    (==) (S n) (S m) = n == m
    (==) _ _ = False

instance Ord Nat where
    (<=) O m = True
    (<=) (S n) (S m) = n <= m
    (<=) _ _ = False
    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.
    
    min :: Nat -> Nat -> Nat
    min O _ = O
    min _ O = O
    min (S n) (S m) = S(min n m)
    
    max :: Nat -> Nat -> Nat
    max O m = m
    max m O = m
    max (S n) (S m) = S(max m n)


----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S n) = odd n

-- Jeito menos eficiente que tambèm funciona
--even (S O) = False
--even (S(S O)) = True
--even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S n) = even n

--odd (S O) = True
--odd (S(S O)) = False
--even (S (S n)) = odd n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) n O = n
(<+>) n (S m)  = S (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) O _ = O
(<->) n O = n
(<->) (S n) (S m) = n <-> m

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) _ O = O
(<*>) n (S  m) = n <+> (n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) _ O = (S O)
(<^>) n (S m) = n <*> (n <^> m)

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) _ O = error "</>: NZD"
(</>) n m 
        | n < m = O
        | otherwise = S((n <-> m) </> m)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ O = error "<&>: NZD"
(<%>) n m
        | n < m = n
        | otherwise = (n <-> m) <%> m

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) m n
        | n <%> m == O = True
        | otherwise = False

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff O m = m
absDiff m O = m
absDiff (S n) (S m) = absDiff n m

(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = (S O)
factorial (S n) = (factorial n) <*> (S n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg _ = (S O)

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ O = error "lo: argument must be greater than O"
lo b a
     | b <= 1 = error "lo: base must be greater than SO"
     | a <= b = O
     | otherwise = S(lo (a </> b) b)


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat n = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = O 
        | x == 0    = O
        | otherwise = S (fromInteger (x - 1))

