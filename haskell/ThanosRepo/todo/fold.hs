module Fold where

initNew :: [a] -> [a]
initNew [] = error "No init in Nil"
initNew [x] = []
initNew (x : xs) = x : init xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (initN xs)
    where initN xs = case xs of
                     [x] -> []
                     (x:xs) -> x : initN xs
