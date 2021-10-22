{-# LANGUAGE InstanceSigs #-}
import Data.Function
import Control.Applicative

import Data.Set (Set, fromList)
import Data.List


-- [Nil]
-- [ 1 :. 2 :. 3 :. Nil ]
-- [ 1 'f' 2 'f' 3 'f' a ]
-- [ 'f' 1 ('f' 2 ('f' 3  a)) ]



-- foldR fun' param List

-- თუ ცარიელია ვაბრუნებთ პარამეტრს, თუ არადა პირველ ელემენტს მასივის

headOrFirst :: a -> [a] -> a
headOrFirst = foldr const

count :: [Integer] -> Integer
count = foldr (+) 0

map' :: (a -> b) -> [a] -> [b]
map' fab = foldr ((:) . fab) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' fab = foldr (\a bs -> if fab a then a : bs else bs) []


data ViewInfo entityTarget view =
    ViewInfo
        { view :: view,
          entityTargets :: Set entityTarget }
    deriving (Eq, Ord, Show)

instance Functor (ViewInfo entityTarget) where
    fmap :: (a -> b) -> ViewInfo entityTarget a -> ViewInfo entityTarget b
    fmap fab ma =
        ViewInfo
            { view = fab (ma & view),
              entityTargets = ma & entityTargets }

    (<$) :: a -> ViewInfo entityTarget b -> ViewInfo entityTarget a
    (<$) = fmap . const

instance (Ord entityTarget) => Applicative (ViewInfo entityTarget) where
    pure :: a -> ViewInfo entityTarget a
    pure a =
        ViewInfo
            { view = a,
              entityTargets = mempty }

    (<*>) :: ViewInfo entityTarget (a -> b) -> ViewInfo entityTarget a -> ViewInfo entityTarget b
    (<*>) mab ma =
        ViewInfo
            { view = (mab & view) (ma & view),
              entityTargets = mappend (mab & entityTargets) (ma & entityTargets) }
        
    -- liftA2 :: (a -> b -> c) -> ViewInfo entityTarget a -> ViewInfo entityTarget b -> ViewInfo entityTarget c


data OptionalTest a =
    Some a
    | None deriving(Eq, Show)

instance Functor OptionalTest where
    fmap :: (a -> b) -> OptionalTest a -> OptionalTest b
    fmap ab fa =
        case fa of
            Some a -> Some (ab a)
            None -> None

    (<$) :: Functor f => a -> f b -> f a
    (<$) a = fmap (const a)


instance Applicative OptionalTest where
    pure :: a -> OptionalTest a
    pure = Some

    (<*>) :: OptionalTest(a -> b) -> OptionalTest a -> OptionalTest b
    (<*>) fab fa =
        case (fab, fa) of
            (Some ab, Some a) -> Some (ab a)
            (_, _) -> None

    liftA2 :: (a -> b -> c) -> OptionalTest a -> OptionalTest b -> OptionalTest c
    -- liftA2 abc = (<*>) . fmap abc
    liftA2 abc = (<*>) . (<*>) (pure abc)

-- Seq ???
sequenceOptional :: [OptionalTest a] -> OptionalTest [a]
sequenceOptional = foldr (liftA2 (:)) (pure [])

-- (a -> b -> c) -> f a -> f b -> f c