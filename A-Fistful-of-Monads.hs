import Control.Monad

----------------------------------
-- Getting our feet wet with Maybe
----------------------------------

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x

-----------------------
-- The Monad type class
-----------------------

--instance Monad Maybe where
--    return x = Just x
--    Nothing >>= f = Nothing
--    Just x >>= f  = f x
--    fail _ = Nothing

----------------
-- Walk the line
----------------

type Birds = Int
type Pole = (Birds,Birds)

--landLeft :: Birds -> Pole -> Pole
--landLeft n (left,right) = (left + n,right)

--landRight :: Birds -> Pole -> Pole
--landRight n (left,right) = (left,right + n)

x -: f = f x

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

--如果不用 Maybe 的话，就需要增加很多麻烦的判断
--routine :: Maybe Pole
--routine = case landLeft 1 (0,0) of
--    Nothing -> Nothing
--    Just pole1 -> case landRight 4 pole1 of
--        Nothing -> Nothing
--        Just pole2 -> case landLeft 2 pole2 of
--            Nothing -> Nothing
--            Just pole3 -> landLeft 1 pole3

--------------
-- do notation
--------------

--foo :: Maybe String
--foo = Just 3   >>= (\x ->
--      Just "!" >>= (\y ->
--      Just (show x ++ y)))

foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)

--嵌套的 lambda 表达式 Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
--顺序的 lambda 表达式 Just 3 >>= (\x -> Just "!") >>= (\y -> Just (show 3 ++ y))
--两者结合的 lambda 表达式 Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))) >>= (\z -> Just (z ++ "!"))
--test = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))) >>= (\z -> Just (z ++ "!"))

test :: Maybe String
test = do
    x <- Just 3
    y <- Just "!"
    z <- Just (show x ++ y)
    Just (z ++ "!")

--routine :: Maybe Pole
--routine = do
--    start <- return (0,0)
--    first <- landLeft 2 start
--    second <- landRight 2 first
--    landLeft 1 second

--routine :: Maybe Pole
--routine =
--    case Just (0,0) of
--        Nothing -> Nothing
--        Just start -> case landLeft 2 start of
--            Nothing -> Nothing
--            Just first -> case landRight 2 first of
--                Nothing -> Nothing
--                Just second -> landLeft 1 second

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landLeft 1 second

justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x

-----------------
-- The list monad
-----------------

--instance Monad [] where
--    return x = [x]
--    xs >>= f = concat (map f xs)
--    fail _ = []

listOfTuples :: [(Int,Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)

--guard :: (MonadPlus m) => Bool -> m ()
--guard True = return ()
--guard False = mzero

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

--moveKnight :: KnightPos -> [KnightPos]
--moveKnight (c,r) = filter onBoard
--    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
--    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
--    ]
--    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

--in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-------------
-- Monad laws
-------------

--Left identity
--return x >>= f is the same damn thing as f x

--Right identity
--m >>= return is no different than just m

--Associativity
--Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)
