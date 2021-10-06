import Data.Function
import Control.Applicative

import Data.Set (Set, fromList)
import Data.List


data Optional a =
    Full a
    | Empty
    deriving (Eq, Show)


(<+>) :: Optional a -> Optional a -> Optional a
(<+>) (Full a) _ = Full a
(<+>) _ (Full a) = Full a
(<+>) Empty Empty = Empty


headOr :: a -> [a] -> a
headOr = foldr const

product' :: Num a => [a] -> a
product' = foldr (*) 1

length' :: Num b => [a] -> b
length' = foldr (\_ -> (+) 1) 0

map' :: (a -> b) -> [a] -> [b]
map' fab = foldr ((:) . fab) []

append :: [a] -> [a] -> [a] -- TODO Check append [1,2] [3,4] -> [3,4,1,2]
append = foldr (:)