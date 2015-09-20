import qualified Data.ByteString.Lazy as B
import Data.Monoid
import Control.Monad.Writer
import Control.Monad
import System.Random
import Control.Monad.State
import Data.List
import Data.Ratio

-----------------------------
-- Writer? I hardly know her!
-----------------------------

--没有附加值
--isBigGang :: Int -> Bool
--isBigGang x = x > 9

--有一个 String 类型的附加值
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

--应用于 String
--applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
--applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

--应用于 [c]
--applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])
--applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

--应用于 Monoid
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

--newtype Writer w a = Writer { runWriter :: (a, w) }

--instance (Monoid w) => Monad (Writer w) where
--    return x = Writer (x, mempty)
--    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

--multWithLog :: Writer [String] Int
--multWithLog = do
--    a <- logNumber 3
--    b <- logNumber 5
--    return (a*b)

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

--gcd' :: Int -> Int -> Int
--gcd' a b
--    | b == 0    = a
--    | otherwise = gcd' b (a `mod` b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcdReverse' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
    tell ["0"]
finalCountDown' x = do
    finalCountDown' (x-1)
    tell [show x]

------------------------------------
-- Reader? Ugh, not this joke again.
------------------------------------

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

addStuff' :: Int -> Int
addStuff' x = let
    a = (*2) x
    b = (+10) x
    in a+b

---------------------------------
-- Tasteful stateful computations
---------------------------------

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

--s -> (a,s)

type Stack = [Int]

--pop :: Stack -> (Int,Stack)
--pop (x:xs) = (x,xs)

--push :: Int -> Stack -> ((),Stack)
--push a xs = ((),a:xs)

--stackManip :: Stack -> (Int, Stack)
--stackManip stack = let
--    ((),newStack1) = push 3 stack
--    (a ,newStack2) = pop newStack1
--    in pop newStack2

--stackManip = do
--    push 3
--    a <- pop
--    pop

--newtype State s a = State { runState :: s -> (a,s) }

--instance Monad (State s) where
--    return x = State $ \s -> (x,s)
--    (State h) >>= f = State $ \s -> let (a, newState) = h s
--                                        (State g) = f a
--                                    in  g newState

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

--stackManip :: State Stack Int
--stackManip = do
--    push 3
--    a <- pop
--    pop

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]

--random :: (RandomGen g, Random a) => g -> (a, g)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins' :: State StdGen (Bool,Bool,Bool)
threeCoins' = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)

--------------------------
-- Error error on the wall
--------------------------

--instance (Error e) => Monad (Either e) where
--    return x = Right x
--    Right x >>= f = f x
--    Left err >>= f = Left err
--    fail msg = Left (strMsg msg)

--------------------------------
-- Some useful monadic functions
--------------------------------

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' mf m = do
    f <- mf
    x <- m
    return (f x)

joinedMaybes :: Maybe Int
joinedMaybes = do
    m <- Just (Just 8)
    m

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9     = Nothing
    | otherwise = Just (acc + x)

--solveRPN :: String -> Double
--solveRPN = head . foldl foldingFunction [] . words

--foldingFunction :: [Double] -> String -> [Double]
--foldingFunction (x:y:ys) "*" = (y * x):ys
--foldingFunction (x:y:ys) "+" = (y + x):ys
--foldingFunction (x:y:ys) "-" = (y - x):ys
--foldingFunction xs numberString = read numberString:xs

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "+" = return ((x + y):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

--上面的 foldr + return 可以用 foldr1 来代替
--inMany x start = return start >>= foldr1 (<=<) (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

----------------
-- Making monads
----------------

--[(3,0.5),(5,0.25),(9,0.25)]
--[(3,1%2),(5,1%4),(9,1%4)]

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [( Prob [('a',1%2),('b',1%2)], 1%4 )
    ,( Prob [('c',1%2),('d',1%2)], 3%4 )
    ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

instance Applicative Prob where
    pure = return
    (<*>) = ap

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])
