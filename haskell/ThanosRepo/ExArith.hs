module ExArith where

data ArEx = Atom Integer
          | Plus ArEx ArEx
          | Times ArEx ArEx
          | Neg ArEx
  deriving (Eq, Show)

-- pretty printer
pretty :: ArEx -> String
pretty (Atom x) = show x
pretty (Plus x y) = "(" ++ (pretty x) ++ "+" ++ (pretty y) ++ ")"
pretty (Times x y) = "(" ++ (pretty x) ++ "*" ++ (pretty y) ++ ")"
pretty (Neg x) = "-" ++ "(" ++ (pretty x) ++ ")"

-- example expressions
ex1 = (Atom 23) `Plus` (Atom 2)
ex2 = (Atom 7) `Times` ((Atom 7) `Plus` ((Atom 2) `Times` (Atom 8)))
ex3 = Times ex1 ex2
ex4 = Neg $ ex3 `Plus` ex1
ex5 = (Neg ex1) `Times` (Neg ex4)

-- eval evaluates an expression and returns its value
eval :: ArEx -> Integer
eval (Atom x) = x
eval (Plus x y) = (eval x) + (eval y)
eval (Times x y) = (eval x) * (eval y)
eval (Neg x) = -(eval x)

-- step should make only 1 step of calculation on a given ArEx
step :: ArEx -> ArEx
step (Atom x) = Atom x
step (Plus x y) = case (x, y) of
        (Atom x, Atom y) -> Atom (x+y)
        (Atom x, y)      -> Plus (Atom x) (step y)
        (x, y)           -> Plus (step x) y
step (Times x y) = case (x, y) of
        (Atom x, Atom y) -> Atom (x*y)
        (Atom x, y)      -> Times (Atom x) (step y)
        (x, y)           -> Times (step x) y
step (Neg x) = case x of
        (Atom x)         -> Atom (-x)
        x                -> Neg (step x) 
