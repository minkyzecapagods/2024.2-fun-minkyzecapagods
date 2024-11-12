module Int where
import Prelude hiding (Int)
import ThanosRepo.ExNat as N

data Int = Int Nat Nat

instance Show Int where
  show (Int n m)
                | isNeg(Int n m) = "-" ++ show (m-n)
                | otherwise = show (m-n)
{--
instance Num Int where
    (+) = soma
    (*) = multi
    (-) = subtr
    abs (Int n m) = (Int (absDiff n m) 0)
    signum = sgInt
    fromInteger x
        | x >= 0     = (Int (N.fromInteger x) 0)
        | otherwise = (Int 0 (N.fromInteger x))
--}
instance Eq Int where
  (==) a b = x == u && y == v
            where (Int x y) = normalize a
                  (Int u v) = normalize b

zero :: Int -> Bool
zero (Int x y) = x == y

isNeg :: Int -> Bool
isNeg (Int x y) = x < y

isPos :: Int -> Bool
isPos (Int x y) = x > y

plusInv :: Int -> Int
plusInv (Int x y) = (Int y x)

-- (a - b) + (a - b) = (a + a) + (- b - b) = (a + a) - (b + b)
add :: Int -> Int -> Int
add (Int x y) (Int u v) = (Int (x+u) (y+v))

-- (a - b) - (a - b) 
sub :: Int -> Int -> Int
sub (Int x y) (Int u v) = (Int (x-y) (u-v))

-- (a - b) * (x - y) = ax - ay - bx + by = (ax + by) + (-ay - bx) = (ax + by) - (ay + bx)
--  ou (ax - ay) + (-bx + by) = (ax - ay) + (by - bx)
mul :: Int -> Int -> Int
mul (Int x y) (Int u v) = add (Int xu xv) (Int yv yu)
              where xu = x*u
                    yv = y*v
                    xv = x*v
                    yu = y*u

sgInt :: Int -> Int
sgInt (Int n m)
            | isPos (Int n m) = (Int (sg (n-m)) O)
            | otherwise = (Int O (S O))

normalize :: Int -> Int
normalize (Int a b)
                | zero(Int a b) = (Int O O)
                | isNeg(Int a b) = (Int O (absDiff a b))
                | otherwise = (Int (a-b) O)

fromInt :: Integral a => Int -> a
fromInt a
         | isNeg a = -(fromNat n)
         | otherwise = (fromNat p)
            where (Int p n) = normalize a


