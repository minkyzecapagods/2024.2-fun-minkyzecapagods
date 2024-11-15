{-# LANGUAGE GADTs #-}

module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head (x:_) = x
head _ = error "head: head not found"

tail :: [a] -> [a]
tail (_:xs) = xs
tail _ = error "tail: tail not found"

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (_:xs) = 1 + (length xs)

sum :: Num a => [a] -> a
sum [] = 0
sum (n:ns) = n + (sum ns)

product :: Num a => [a] -> a
product [] = 1
product (n:ns) = n * (product ns)

reverse :: [a] -> [a]
reverse (x:xs) = reverse xs ++ [x]
reverse [] = []
-- reverse (x:xs) = reverse xs ++ x

(++) :: [a] -> [a] -> [a]
(++) (x:xs) ys = x:xs ++ ys
(++) _ ys = ys

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc a [] = [a]
snoc a (y:ys) = y:(snoc a ys)

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm??)
infixl 5 +++

--max value of a list
maximum :: Ord a => [a] -> a
maximum [n] = n
maximum (m:n:ns)
                | m > n = maximum (m:ns)
                | otherwise = maximum (n:ns)
maximum _ = error "Nil doesn't have max"

-- min value of a list
minimum :: Ord a => [a] -> a
minimum [n] = n
minimum (m:n:ns)
                | m < n = minimum (m:ns)
                | otherwise = minimum (n:ns)
minimum _ = error "Nil doesn't have min"

-- take
take :: Int -> [a] -> [a]
take _ [] = []
take n (l:ls)
            | n > 0 = l : take (n-1) ls
            | otherwise = []

-- drop
drop :: Int -> [a] -> [a]
drop _ [] = []
drop n l@(_:ls)
            | n > 0 = drop (n-1) ls
            | otherwise = l

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (l:ls)
                 | p l = l : takeWhile p ls
                 | otherwise = []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (l:ls)
                 | p l = dropWhile p ls
                 | otherwise = (l:ls)

-- tails
tails :: [a] -> [[a]]
tails [] = []
tails l@(_:ls) = tail l : tails ls

-- init
innit :: [a] -> [a]
innit [a] = []
innit (l:ls) = l : (innit ls)
innit _ = []

-- inits
innits :: [a] -> [[a]]
innits [] = [[]]
innits l@(_:ls) = ini : innits ini
                  where ini = innit l

-- subsequences
{--subsequences :: [a] -> [[a]]
subsequences [] = []
subsequences (n:ns)
--}
-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter p (l:ls)
              | p l = l : filter p ls
              | otherwise = filter p ls
filter _ _ = []

-- map
map :: (a -> b) -> [a] -> [b]
map p (l:ls) = p l : map p ls
map _ _ = []

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf

-- isSuffixOf

-- zip
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []

z :: [a] -> [b] -> [(a,b)]
z = zipWith tupla

tupla :: a -> b -> (a,b)
tupla x y = (x, y)

-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

zW :: (a -> b -> c) -> [a] -> [b] -> [c]
zW f xs ys= map (uncurry f) (zip xs ys)

-- intercalate
-- nub

-- splitAt
splitAt :: Int -> [a] -> ([a], [a])
splitAt _ [] = ([],[])
splitAt i (l:ls)
                | i > 0 = ((l:ms), ns)
                | otherwise = ([], (l:ls))
                  where (ms,ns) = splitAt (i-1) ls

-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
-- Res : Percorre as mesmas n posições da lista duas vezes, uma para dropar e outra para takear.

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

--fold

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

