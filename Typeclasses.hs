----------
-- Functor
----------

class Functor f where
    fmap :: (a -> b) -> f a -> f b

----------------------
-- Applicative Functor
----------------------

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

--------
-- Monad
--------

class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg
