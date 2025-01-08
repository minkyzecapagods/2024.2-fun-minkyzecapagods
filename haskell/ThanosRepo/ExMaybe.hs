module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x : xs) = case x of
                    Just x -> x : (catMaybes xs)
                    Nothing -> catMaybes xs

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "There is nothing here"

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just y) = y
fromMaybe x _ = x

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe = undefined

justMap :: (a -> Maybe b) -> [a] -> [b]
justMap _ [] = []
justMap f (m : ms) = case f m of
                     Just b -> b : justMap f ms
                     Nothing -> justMap f ms

maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just x) = f x
maybe y _ _ = y

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x


tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith [] xs = xs
tryToModifyWith _ [] = []
tryToModifyWith (f:fs) (x:xs) = case f of
                          Just f -> (f x) : tryToModifyWith fs xs
                          Nothing -> tryToModifyWith fs xs

