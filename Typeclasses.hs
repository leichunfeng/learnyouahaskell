----------
-- Functor
----------

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor ((->) r) where
    fmap = (.)

--lifting a function

----------------------
-- Applicative Functor
----------------------

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

---------
-- Monoid
---------

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r
