{-# LANGUAGE InstanceSigs #-}
import Data.Function
import Control.Applicative

import Data.Set (Set, fromList)
import Data.List


-- data ViewInfo entityTarget view =
--     ViewInfo
--         { view :: view,
--           entityTargets :: Set entityTarget }
--     deriving (Eq, Ord, Show)

-- instance Functor (ViewInfo entityTarget) where
--     fmap :: (a -> b) -> ViewInfo entityTarget a -> ViewInfo entityTarget b
--     fmap mapping viewInfo =
--         ViewInfo
--             { view = viewInfo & view & mapping,
--               entityTargets = viewInfo & entityTargets }

--     (<$) :: Functor f => a -> f b -> f a
--     (<$) = fmap . const



data Optional a =
    Full a
    | Empty
    deriving (Eq, Show)

instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap mapping optional =
        case optional of
            Full a -> Full (mapping a)
            Empty -> Empty

    (<$) :: Functor f => a -> f b -> f a
    (<$) = fmap . const


instance Applicative Optional where

    pure :: a -> Optional a
    pure = Full

    (<*>) ::
        Optional(a -> b)
        -> Optional a
        -> Optional b
    (<*>) fab fa =
        case (fab, fa) of
            (Full ab, Full a) -> Full (ab a)
            (_, _) -> Empty

    liftA2 ::
        (a -> b -> c)
        -> Optional a
        -> Optional b
        -> Optional c
    liftA2 abc =
        -- (<*>) . (<*>) (pure abc)
        (<*>) . fmap abc