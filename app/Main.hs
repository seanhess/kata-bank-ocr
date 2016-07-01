module Main where

import Lib

main :: IO ()
main = do
    useCase3 testLines

useCase3 :: String -> IO ()
useCase3 input = do
    mapM_ putStrLn $ map (showResult . parseDigits . chunkInputDigits) $ readInput input


