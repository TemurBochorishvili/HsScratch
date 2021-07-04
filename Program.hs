-- ($)   :: (a -> b) -> a -> b -------- map
-- pure  :: a -> f a
-- (<$>) :: (a -> b) -> f a -> f b -------- fmap
-- (<*>) :: f (a -> b) -> f a -> f b ------ apply
-- (>>=) :: m a -> (a -> m b) -> m b ------ bind
-- const :: a -> b -> a -- returns first argument
-- ?Pointless? const
-- ?Pointfull? const

import Data.Function
import Control.Applicative

import Data.Set
import Data.List


data ViewInfo entityTarget view =
    ViewInfo
        { view :: view,
          entityTargets :: Set entityTarget }
    deriving (Eq, Ord, Show)

instance Functor (ViewInfo entityTarget) where
    fmap mapping viewInfo =
        ViewInfo
            { view = viewInfo & view & mapping,
              entityTargets = viewInfo & entityTargets }

instance (Ord entityTarget) => Applicative (ViewInfo entityTarget) where
    pure view =
        ViewInfo
            { view = view,
              entityTargets = mempty }

    ( <*> ) appling viewInfo =
        ViewInfo
            { view = (appling & view) (viewInfo & view),
              entityTargets = mappend (appling & entityTargets) (viewInfo & entityTargets) }

    liftA2 f viewInfo1 viewInfo2 =
        ViewInfo
            { view = f (viewInfo1 & view) (viewInfo2 & view),
              entityTargets = mappend (viewInfo1 & entityTargets) (viewInfo2 & entityTargets) }

instance (Ord entityTarget) => Monad (ViewInfo entityTarget) where
    ( >>= ) viewInfo binding =
        ViewInfo
            { view = resultViewInfo & view,
              entityTargets = mappend (viewInfo & entityTargets) (resultViewInfo & entityTargets) }
        where resultViewInfo = viewInfo & view & binding

data AppEntityTarget
    = Court Int
    | Employee String
    | HighCouncilOfJustice
    deriving (Eq, Ord, Show)

-- foldRight f a xs
-- [ 1 :. 2 :. 3 :. Nil ]
-- [ 1 `f` 2 `f` 3 `f` a]
-- [ f 1 (f 2 (f 3 a)) ]

-- headOr a xs = foldRight (\x _ -> x) a xs 
-- headOr a xs = foldRight const a xs
-- headOr = foldRight const

-- product xs = foldRight (*) 1 xs
-- product = foldRight (*) 1

-- sum xs = foldRight (+) 0 xs
-- sum = foldRight (+) 0

-- length = foldRight (\_ b -> b + 1) 0
-- length = foldRight (const (1 +)) 0 // TODO ვერ გავიგე 
-- flip ანაცვლებს იმ ფუნქციის არგუმენტებს რომელზეც მოსდებ.
    -- const ------ a -> b -> a
    -- flip const - b -> c -> c

-- map f = foldRight (\a bs -> f a :. bs) Nil
-- map f = foldRight (\a -> (f a :.)) Nil
-- map f = foldRight ((:.) . f) Nil
-- 1 (+ 1 :.) 2 (+ 1 :.) 3 (+ 1 :.) Nil
-- [ f 1 (f 2 (f 3 a)) ]

-- filter p = foldRight (\a as -> if p a then a :. as else as ) Nil
-- append (++) xs ys = foldRight (:.) ys xs
-- append (++) = foldRight (:.) 
-- append (++) = flip foldRight (:.) flip რატო?
-- flatten 



headOr :: a -> List a -> a
headOr a (x :. xs) =
    _todo1
headOr a Nil =
    _todo2


viewInfo1 =
    ViewInfo
        { view = "vitom",
          entityTargets =
            fromList
                [ Court 1,
                  Court 2,
                  Employee "Kote Zibzibadze",
                  HighCouncilOfJustice ] }

viewInfo2 =
    ViewInfo
        { view = 10,
          entityTargets =
            fromList
                [ Court 14,
                  Court 32,
                  Employee "Kote Zibzibadze",
                  Employee "Kavil Kavilovichi",
                  HighCouncilOfJustice ] }