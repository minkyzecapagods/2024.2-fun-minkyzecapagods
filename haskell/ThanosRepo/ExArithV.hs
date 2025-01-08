module ExArithV where

-- modify ExArith to allow for variables

-- decide how to represent Assignments:
type Assignment = [(String, Integer)]

data ArExV = Atom Integer
           | Var String
           | Plus ArExV ArExV
           | Times ArExV ArExV
           | Neg ArExV
  deriving (Eq, Show)

-- pretty printer
pretty :: ArExV -> String
pretty (Atom x) = show x
pretty (Var v) = v
pretty (Plus x y) = "(" ++ (pretty x) ++ "+" ++ (pretty y) ++ ")"
pretty (Times x y) = "(" ++ (pretty x) ++ "*" ++ (pretty y) ++ ")"
pretty (Neg x) = "-" ++ "(" ++ (pretty x) ++ ")" 

-- eval evialuates an expression and returns its value
-- eval :: ?
-- ASSignment hahaha
eval :: Assignment -> ArExV -> Integer
eval _ (Atom x) = x
eval ass (Var v) = case varValue ass v of
                  Just x -> x
                  Nothing -> error (v ++ " is not in scope")
eval ass (Plus x y) = (eval ass x) + (eval ass y)
eval ass (Times x y) = (eval ass x) * (eval ass y)
eval ass (Neg x) = -(eval ass x)

varValue :: Assignment -> String -> Maybe Integer
varValue [] _ = Nothing
varValue ((s, i) : vs) v
                  | v == s = Just i
                  | otherwise = varValue vs v

