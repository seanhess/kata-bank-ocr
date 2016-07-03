module Lib where

-- import Debug.Trace (trace)
import Data.List (zip3, intercalate)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromMaybe, catMaybes, isNothing, mapMaybe)


-- a parsed AccountNumber. Legible but not necessarily valid
-- it has exactly nine digits
-- we could use [Int] or Int but let's see if this works better
data AccountNumber = AccountNumber Int Int Int Int Int Int Int Int Int
                   deriving (Eq, Show)


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


parseInputDigits :: [InputDigit] -> EntryNumber
parseInputDigits = map parseInputDigit

parseInputDigit :: InputDigit -> Maybe Int
parseInputDigit (l1, l2, l3)
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



-- user story 3 / results ------------------------------------------

showResult :: [Maybe SquareDigit] -> String
showResult entry =
    -- completely invalid input, not enough characters, etc
    if any isNothing entry
      then ill entry
      else
        let ds = catMaybes entry in

        if isValid ds
          then ok entry
          else

            let combos = allPossibleCombos ds
            in case filter isValid combos of
                [] -> ill entry
                [n] ->
                  if isValid n
                    then ok (map Just n)
                    else err entry
                ns -> showNumber entry ++ " AMB " ++ show (map showAccount $ mapMaybe accountNumber ns)

  where
    isValid ds = (fmap checkSumValid $ accountNumber ds) == Just True
    ill ds = showNumber ds ++ " ILL"
    err ds = showNumber ds ++ " ERR"
    ok ds = showNumber ds
    showAccount (AccountNumber d9 d8 d7 d6 d5 d4 d3 d2 d1) =
      foldl (++) "" [show d9, show d8, show d7, show d6, show d5, show d4, show d3, show d2, show d1]

showNumber :: [Maybe SquareDigit] -> String
showNumber = foldl (++) "" . map showDigit
  where
    showDigit md = fromMaybe "?" $ do
      d <- md
      n <- parseSquareDigit d
      return $ show n

showAccFlag :: AccountNumber -> String
showAccFlag acc
  | checkSumValid acc = ""
  | otherwise = " ERR"






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


-- user story 4 -----------------------------------------------------

-- we need a better representation of the input digit
-- it's really a 3x3 matrix with True or False in it
-- and we ignore the top right and top left
-- _x_
-- xxx
-- _xx

data SquareDigit = SquareDigit
    {             tm :: Bool
    , ml :: Bool, mm :: Bool, mr :: Bool
    , bl :: Bool, bm :: Bool, br :: Bool
    } deriving (Eq, Show)

-- instance Show SquareDigit where
--     show (SquareDigit tm' ml' mm' mr' bl' bm' br') =
--         "\n" ++ intercalate "\n"
--           [ [' ', under tm', ' ']
--           , [ pipe ml', under mm', pipe mr']
--           , [ pipe bl', under bm', pipe br']
--           ]
--       where
--         under x = if x then '_' else ' '
--         pipe x = if x then '|' else ' '

fromInputDigit :: InputDigit -> Maybe SquareDigit
fromInputDigit ([_, tm', _], [ml', mm', mr'], [bl', bm', br']) =
    Just $ SquareDigit (tm' == '_') (ml' == '|') (mm' == '_') (mr' == '|') (bl' == '|') (bm' == '_') (br' == '|')
fromInputDigit _ = Nothing

toInputDigit :: SquareDigit -> InputDigit
toInputDigit d =
    ( [' ', under (tm d), ' ']
    , [ pipe (ml d), under (mm d), pipe (mr d)]
    , [ pipe (bl d), under (bm d), pipe (br d)]
    )
  where
    under x = if x then '_' else ' '
    pipe x = if x then '|' else ' '



-- flip each bit with the rest constant
possibleDigits :: SquareDigit -> [SquareDigit]
possibleDigits d =
    [ d { tm = not (tm d) }
    , d { ml = not (ml d) }
    , d { mm = not (mm d) }
    , d { mr = not (mr d) }
    , d { bl = not (bl d) }
    , d { bm = not (bm d) }
    , d { br = not (br d) }
    ]

parseSquareDigits :: [SquareDigit] -> EntryNumber
parseSquareDigits = map parseSquareDigit


parseSquareDigit :: SquareDigit -> Maybe Int
parseSquareDigit = parseInputDigit . toInputDigit


allPossibleCombos :: [SquareDigit] -> [[SquareDigit]]
allPossibleCombos = go []
  where
    go :: [SquareDigit] -> [SquareDigit] -> [[SquareDigit]]
    go prefix (d:ds) =
      map (combine prefix ds) (possibleDigits d)
      ++ go (prefix ++ [d]) ds
    go _ [] = []

    combine prefix suffix d = prefix ++ [d] ++ suffix


accountNumber :: [SquareDigit] -> Maybe AccountNumber
accountNumber = fromEntryNum . parseSquareDigits





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

dumpSquareDigits :: [SquareDigit] -> IO ()
dumpSquareDigits ds =
    putStrLn $ dumpDigits $ map toInputDigit ds

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
  ]

testCaseIll = intercalate "\n"
  [ "    _  _  _  _  _  _     _ "
  , "|_||_|| || ||_   |  |  | _ "
  , "  | _||_||_||_|  |  |  | _|"
  ]

testCaseErr = intercalate "\n"
  [ "    _  _     _  _  _  _  _ "
  , "  | _| _||_||_ |_   ||_||_|"
  , "  ||_  _|  | _||_|  ||_||_|"
  ]

testCaseFour = intercalate "\n"
  [ "    _  _     _  _  _  _  _ "
  , "  | _| _||_| _ |_   ||_||_|"
  , "  ||_  _|  | _||_|  ||_| _ "
  ]

testCaseAMB1 = intercalate "\n"
  [ "                           "
  , "  |  |  |  |  |  |  |  |  |"
  , "  |  |  |  |  |  |  |  |  |"
  ]

testCaseAMB3 = intercalate "\n"
  [ " _  _  _  _  _  _  _  _  _ "
  , " _| _| _| _| _| _| _| _| _|"
  , " _| _| _| _| _| _| _| _| _|"
  ]

testCaseAMB8 = intercalate "\n"
  [ " _  _  _  _  _  _  _  _  _ "
  , "|_||_||_||_||_||_||_||_||_|"
  , "|_||_||_||_||_||_||_||_||_|"
  ]

testCaseAMBN = intercalate "\n"
  [ "    _  _  _  _  _  _     _ "
  , "|_||_|| || ||_   |  |  ||_ "
  , "  | _||_||_||_|  |  |  | _|"
  ]


testLines = intercalate "\n                           \n" [testCase, testCaseIll, testCaseErr, testCaseFour, testCaseAMB1, testCaseAMB3, testCaseAMB8, testCaseAMBN]


acc1 = AccountNumber 4 5 7 5 0 8 0 0 0
acc2 = AccountNumber 6 6 4 3 7 1 4 9 5
