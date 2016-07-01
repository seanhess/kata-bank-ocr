module Lib where

import Data.List (zip3, intercalate)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromMaybe)


-- a parsed AccountNumber. Legible but not necessarily valid
-- it has exactly nine digits
-- we could use [Int] or Int but let's see if this works better
data AccountNumber = AccountNumber Int Int Int Int Int Int Int Int Int
                   deriving (Eq)


-- a parsed entry that may or may not be valid or legible
type EntryNumber = [Maybe Int]


type InputLine = String
type InputDigit = (String, String, String)



-- parsing -------------------------------------------------

readInput :: String -> [[String]]
readInput = map (take 3) . chunksOf 4 . splitOn "\n"


chunkInputDigits :: [String] -> [InputDigit]
chunkInputDigits [l1, l2, l3] =
    zip3 (chunksOf 3 l1) (chunksOf 3 l2) (chunksOf 3 l3)
chunkInputDigits _ = []


parseDigits :: [InputDigit] -> EntryNumber
parseDigits = map parseDigit


parseDigit :: InputDigit -> Maybe Int
parseDigit (l1, l2, l3)
  | digit == zero  = Just 0
  | digit == one   = Just 1
  | digit == two   = Just 2
  | digit == three = Just 3
  | digit == four  = Just 4
  | digit == five  = Just 5
  | digit == six   = Just 6
  | digit == seven = Just 7
  | digit == eight = Just 8
  | digit == nine  = Just 9
  | otherwise = Nothing
  where
    digit = [l1, l2, l3]



-- user story 2 ----------------------------------------------------

checkSumValid :: AccountNumber -> Bool
checkSumValid (AccountNumber d9 d8 d7 d6 d5 d4 d3 d2 d1) =
    ((1*d1 + 2*d2 + 3*d3 + 4*d4 + 5*d5 + 6*d6 + 7*d7 + 8*d8 + 9*d9) `mod` 11 == 0)



-- user story 3 ----------------------------------------------------

showResult :: EntryNumber -> String
showResult entry = showEntryNum entry ++ showEntryFlag entry

showEntryNum :: EntryNumber -> String
showEntryNum = foldl (++) "" . map showEntryDigit
  where
    showEntryDigit = fromMaybe "?" . fmap show

fromEntryNum :: EntryNumber -> Maybe AccountNumber
fromEntryNum [md9, md8, md7, md6, md5, md4, md3, md2, md1] = do
    d9 <- md9
    d8 <- md8
    d7 <- md7
    d6 <- md6
    d5 <- md5
    d4 <- md4
    d3 <- md3
    d2 <- md2
    d1 <- md1
    return $ AccountNumber d9 d8 d7 d6 d5 d4 d3 d2 d1
fromEntryNum _ = Nothing

showEntryFlag :: EntryNumber -> String
showEntryFlag = fromMaybe " ILL" . fmap showAccFlag . fromEntryNum

showAccFlag :: AccountNumber -> String
showAccFlag acc
  | checkSumValid acc = ""
  | otherwise = " ERR"



-- test cases and debugging ---------------------------------------

digitList :: InputDigit -> [String]
digitList (l1, l2, l3) = [l1, l2, l3]

dumpDigits :: [InputDigit] -> String
dumpDigits = intercalate "\n" . foldl addDigit []
  where
    addDigit :: [String] -> InputDigit -> [String]
    addDigit [d1, d2, d3] (l1, l2, l3) =
      [d1 ++ l1, d2 ++ l2, d3 ++ l3]
    addDigit _ (l1, l2, l3) = [l1, l2, l3]

zero =
  [ " _ "
  , "| |"
  , "|_|"
  ]

one =
  [ "   "
  , "  |"
  , "  |"
  ]

two =
  [ " _ "
  , " _|"
  , "|_ "
  ]

three =
 [ " _ "
 , " _|"
 , " _|"
 ]

four =
  [ "   "
  , "|_|"
  , "  |"
  ]

five =
  [ " _ "
  , "|_ "
  , " _|"
  ]

six =
  [ " _ "
  , "|_ "
  , "|_|"
  ]

seven =
  [ " _ "
  , "  |"
  , "  |"
  ]

eight =
  [ " _ "
  , "|_|"
  , "|_|"
  ]

nine =
  [ " _ "
  , "|_|"
  , " _|"]


testCase = intercalate "\n"
  [ "    _  _     _  _  _  _  _ "
  , "  | _| _||_||_ |_   ||_||_|"
  , "  ||_  _|  | _||_|  ||_| _|"
  , "                           "
  ]

testCaseIll = intercalate "\n"
  [ "    _  _  _  _  _  _     _ "
  , "|_||_|| || ||_   |  |  | _ "
  , "  | _||_||_||_|  |  |  | _|"
  , "                           "
  ]

testCaseErr = intercalate "\n"
  [ "    _  _     _  _  _  _  _ "
  , "  | _| _||_||_ |_   ||_||_|"
  , "  ||_  _|  | _||_|  ||_||_|"
  , "                           "
  ]

testLines = intercalate "\n" [testCase, testCaseIll, testCaseErr]


acc1 = AccountNumber 4 5 7 5 0 8 0 0 0
acc2 = AccountNumber 6 6 4 3 7 1 4 9 5
