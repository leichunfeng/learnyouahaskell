import Control.Monad
import Control.Applicative
import Data.Monoid
import qualified Data.Foldable as F

-----------------
-- Functors redux
-----------------

--instance Functor IO where
--    fmap f action = do
--        result <- action
--        return (f result)

--main = do line <- getLine
--          let line' = reverse line
--          putStrLn $ "You said " ++ line' ++ " backwards!"
--          putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"

main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

--instance Functor ((->) r) where
--    fmap f g = (\x -> f (g x))

--instance Functor (r ->) where
--    fmap f g = (\x -> f (g x))

--instance Functor ((->) r) where
--    fmap = (.)

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

-----------------------
-- Applicative functors
-----------------------

--class (Functor f) => Applicative f where
--    pure :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b

--instance Applicative Maybe where
--    pure = Just
--    Nothing <*> _ = Nothing
--    (Just f) <*> something = fmap f something

----------------------
-- The newtype keyword
----------------------

--data ZipList a = ZipList [a]
data ZipList a = ZipList { getZipList :: [a] }

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

--data CoolBool = CoolBool { getCoolBool :: Bool }
newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

----------
-- Monoids
----------

--instance Monoid [a] where
--    mempty = []
--    mappend = (++)

--lengthCompare :: String -> String -> Ordering
--lengthCompare x y = let a = length x `compare` length y
--                        b = x `compare` y
--                    in  if a == EQ then b else a

--lengthCompare :: String -> String -> Ordering
--lengthCompare x y = (length x `compare` length y) `mappend`
--                    (x `compare` y)

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")

--instance Monoid a => Monoid (Maybe a) where
--    mempty = Nothing
--    Nothing `mappend` m = m
--    m `mappend` Nothing = m
--    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

--newtype First a = First { getFirst :: Maybe a }
--    deriving (Eq, Ord, Read, Show)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )

--前序遍历
instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

--中序遍历
--instance F.Foldable Tree where
--    foldMap f Empty = mempty
--    foldMap f (Node x l r) = f x           `mappend`
--                             F.foldMap f l `mappend`
--                             F.foldMap f r

--后序遍历
--instance F.Foldable Tree where
--    foldMap f Empty = mempty
--    foldMap f (Node x l r) = F.foldMap f r `mappend`
--    	                     F.foldMap f l `mappend`
--                             f x
