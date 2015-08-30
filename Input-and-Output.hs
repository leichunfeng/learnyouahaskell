import Data.Char
import Control.Monad
import System.Random
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

----------------
-- Hello, world!
----------------

--main = putStrLn "hello, world"

--main = do
--    putStrLn "Hello, what's your name?"
--    name <- getLine
--    putStrLn ("Hey " ++ name ++ ", you rock!")

--main = do
--    foo <- putStrLn "Hello, what's your name?"
--    name <- getLine
--    putStrLn ("Hey " ++ name ++ ", you rock!")

--main = do
--    putStrLn "Hello, what's your name?"
--    name <- getLine
--    putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name

--main = do
--    putStrLn "What's your first name?"
--    firstName <- getLine
--    putStrLn "What's your last name?"
--    lastName <- getLine
--    let bigFirstName = map toUpper firstName
--        bigLastName = map toUpper lastName
--    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

--main = do
--    line <- getLine
--    if null line
--        then return ()
--        else do
--            putStrLn $ reverseWords line
--            main

--reverseWords :: String -> String
--reverseWords = unwords . map reverse . words

--main = do
--    return ()
--    return "HAHAHA"
--    line <- getLine
--    return "BLAH BLAH BLAH"
--    return 4
--    putStrLn line

--main = do
--    a <- return "hell"
--    b <- return "yeah!"
--    putStrLn $ a ++ " " ++ b

--main = do
--    let a = "hell"
--        b = "yeah"
--    putStrLn $ a ++ " " ++ b

--main = do   putStr "Hey, "
--            putStr "I'm "
--            putStrLn "Andy!"

--main = do   putChar 't'
--            putChar 'e'
--            putChar 'h'

--putStr :: String -> IO ()
--putStr [] = return ()
--putStr (x:xs) = do
--    putChar x
--    putStr xs

--main = do   print True
--            print 2
--            print "haha"
--            print 3.2
--            print [3,4,3]

--main = do
--    c <- getChar
--    if c /= ' '
--        then do
--            putChar c
--            main
--        else return ()

--main = do
--    c <- getChar
--    when (c /= ' ') $ do
--        putChar c
--        main

--main = do
--    a <- getLine
--    b <- getLine
--    c <- getLine
--    print [a,b,c]

--main = do
--    rs <- sequence [getLine, getLine, getLine]
--    print rs

--main = forever $ do
--    putStr "Give me some input: "
--    l <- getLine
--    putStrLn $ map toUpper l

--main = do
--    colors <- forM [1,2,3,4] (\a -> do
--        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
--        color <- getLine
--        return color)
--    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
--    mapM putStrLn colors

--------------------
-- Files and streams
--------------------

--main = do
--    withFile "something.txt" ReadMode (\handle -> do
--        contents <- hGetContents handle
--        putStr contents)

main = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)

-------------------------
-- Command line arguments
-------------------------

-------------
-- Randomness
-------------

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

--finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
--finiteRandoms 0 gen = ([], gen)
--finiteRandoms n gen =
--    let (value, newGen) = random gen
--        (restOfList, finalGen) = finiteRandoms (n-1) newGen
--    in  (value:restOfList, finalGen)

--------------
-- Bytestrings
--------------

-------------
-- Exceptions
-------------
