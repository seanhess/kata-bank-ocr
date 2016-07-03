module Main where

import Lib
import Prelude hiding (lines)

main :: IO ()
main = do
    useCase3 testLines

useCase3 :: String -> IO ()
useCase3 input = do
    let lines = map chunkInputDigits $ readInput input :: [[InputDigit]]
        digits = map (map fromInputDigit) lines :: [[Maybe SquareDigit]]
    mapM_ putStrLn $ map showResult digits
    return ()
    -- mapM_ putStrLn $ map (showResult . map fromInputDigit . chunkInputDigits) $ readInput input


