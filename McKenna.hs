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

sum' :: [Integer] -> Integer
sum' = foldr (+) 0

product' :: Num a => [a] -> a
product' = foldr (*) 1

length' :: Num b => [a] -> b
-- length' = foldr (\_ -> (1 +)) 0
length' = foldr (const (1 +)) 0 -- ???

map' :: (a -> b) -> [a] -> [b]
-- map' fab = foldr ((:) . fab) []
map' fab = foldr (\a bs -> fab a : bs) []

append :: [a] -> [a] -> [a] -- flip?
-- append xs ys = foldr (:) ys xs
append = flip (foldr (:))

flatten :: [[a]] -> [a]
flatten = foldr append []

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f = flatten . map f

lift2 :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
lift2 fabc (Full a) (Full b) = Full (fabc a b)
lift2 fabc _ _ = Empty

seqOptional :: [Optional a] -> Optional [a]
seqOptional = foldr (lift2 (:)) (Full [])

find' :: (a -> Bool) -> [a] -> Optional a
find' f = foldr (\a bs -> if f a then Full a else bs) Empty

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
