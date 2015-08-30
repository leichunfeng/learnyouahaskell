import System.Environment
import System.IO
import System.Directory

--main = do (fileName:_) <- getArgs
--          contents <- readFile fileName
--          putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName
          if fileExists
              then do contents <- readFile fileName
                      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
              else do putStrLn "The file doesn't exist!"
