module Day8
    ( day8a
    , day8b
    ) where

import MyPrelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

-- import Utils


-- | Answer: 349
day8a :: IO ()
day8a = do
    entries :: [DisplayEntry] <- day8_readInput "data/day8a_input.txt"

    putText "Display Entries:"
    traverse_ print entries
    putText ""

    print $ day8a_compute entries

day8a_compute :: [DisplayEntry] -> Int
day8a_compute !entries =
    length $
    filter isUniquePatternSize $
    concatMap outputDigits entries

type Signal = Char
type Digit = Int

type Pattern = Set Signal

-- | Returns True if the digit can be identified by the number
-- of segments in the pattern (1, 4, 7, 8).
isUniquePatternSize :: Pattern -> Bool
isUniquePatternSize !pattern =
    Set.size pattern `elem` [ 2 -- 2 segments for a 1
                            , 4 -- 4 segemnts for a 4
                            , 3 -- 3 segemnts for a 7
                            , 7 -- 7 segemnts for a 8
                            ]

data DisplayEntry =
    DisplayEntry {
        signalPatterns :: ![Pattern]
      , outputDigits   :: ![Pattern]
    }

instance Show DisplayEntry where
    show DisplayEntry{..} =
        T.unpack $
        mconcat [
            T.intercalate " " $ showPattern <$> signalPatterns
          , " | "
          , T.intercalate " " $ showPattern <$> outputDigits

        ]

showPattern :: Pattern -> Text
showPattern !pattern =
    T.pack $
    Set.toList pattern

patternFromText :: Text -> Pattern
patternFromText !patternTxt =
    Set.fromList $
    T.unpack patternTxt

day8_readInput :: FilePath -> IO [DisplayEntry]
day8_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    return $ mapMaybe day8a_parseLine fileLines

-- | Example:  acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
day8a_parseLine :: Text -> Maybe DisplayEntry
day8a_parseLine !txt
    | [signalPatternsTxt, outputDigitsText] <- T.splitOn " | " txt
    , signalPatterns <- patternFromText <$> T.splitOn " " signalPatternsTxt
    , outputDigits   <- patternFromText <$> T.splitOn " " outputDigitsText
    = Just DisplayEntry{..}
    | otherwise
    = Nothing

{--

  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc

be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb
1  8                     4                             7 
-}


translateDisplayEntryOutput_NEW :: DisplayEntry -> Int
translateDisplayEntryOutput_NEW DisplayEntry{..} =
    let
        allDigs :: Set Int
        allDigs = Set.fromList [0..9]

        -- Start with all possible digits for each pattern.
        patternPossibilities0 :: Map Pattern (Set Int)
        patternPossibilities0 =
            Map.fromList $
            map (,allDigs) signalPatterns

        -- Limit the digit options based on the length of each pattern.
        patternPossibilities1 :: Map Pattern (Set Int)
        patternPossibilities1 =
            Map.mapWithKey (\ptrn digs ->
                                Set.filter (\d -> nSegmentsForDigit d == Set.size ptrn) digs
                           ) patternPossibilities0

        extractSinglePatternForDigit :: Map Pattern (Set Int) -> Int -> Pattern
        extractSinglePatternForDigit !patternPossibilities !dig =
            case find (\(_p,ds) -> ds == Set.singleton dig) $ Map.toList patternPossibilities of
            Nothing -> panic $ "could not find a pattern with the single possible digit: " <> show dig
            Just (p,_ds) -> p

        pattern1 = extractSinglePatternForDigit patternPossibilities1 1
        pattern4 = extractSinglePatternForDigit patternPossibilities1 4
        -- pattern7 = extractSinglePatternForDigit patternPossibilities1 7
        -- pattern8 = extractSinglePatternForDigit patternPossibilities1 8

        -- If we know the signals corresponding to digit segments, then filter the digit possibilities for each
        -- signal pattern based on whether the signal pattern contains the signals and whether the digits contain
        -- the correponding segments.
        filterBySegmentPresence :: Pattern -> Pattern -> Map Pattern (Set Int) -> Map Pattern (Set Int)
        filterBySegmentPresence !signalPattern !digitSegments !patternPossibs =
            Map.mapWithKey (\ptrn digs ->
                                let
                                    signalContainsPattern :: Bool
                                    signalContainsPattern = signalPattern `Set.isSubsetOf` ptrn
                                in
                                    Set.filter (\d -> digitContainsSegments d digitSegments == signalContainsPattern) digs
                           ) patternPossibs        

        -- Filter down the options based on which digits contain the 2 segments for a "1".
        patternPossibilities2 :: Map Pattern (Set Int)
        patternPossibilities2 =
            filterBySegmentPresence pattern1 (lookupDigitSegments 1) patternPossibilities1

        -- Filter down the options based on which digits contain the top-left and middle segments.
        -- We can work out the signals for this by taking the '4' pattern and removing the '1' pattern signals.
        patternPossibilities3 :: Map Pattern (Set Int)
        patternPossibilities3 =
            filterBySegmentPresence (pattern4 `Set.difference` pattern1)
                                    (lookupDigitSegments 4 `Set.difference` lookupDigitSegments 1)
                                    patternPossibilities2

        -- Fetch the single digit possiblity for a given pattern.
        -- Throw an error if there are still multiple possibilities.
        lookupDigitForSignalPattern :: Pattern -> Int
        lookupDigitForSignalPattern !sigPattern
            | Just possibs <- Map.lookup sigPattern patternPossibilities3
            , [singlePoss] <- Set.toList possibs
            = singlePoss
            | otherwise
            = panic $ "could not find a single possible digit for signal pattern: " <> show sigPattern

        translatedDigits :: [Int]
        translatedDigits = lookupDigitForSignalPattern <$> outputDigits

        outputVal :: Int
        outputVal =
            foldl' (\cur dig -> cur * 10 + dig) 0 translatedDigits
    in
        -- traceMap "patternPossibilities0 = " patternPossibilities0 `seq`
        -- traceMap "patternPossibilities1 = " patternPossibilities1 `seq`
        -- traceVal "pattern1 = " pattern1 `seq`
        -- traceVal "pattern4 = " pattern4 `seq`
        -- traceVal "pattern7 = " pattern7 `seq`
        -- traceVal "pattern8 = " pattern8 `seq`
        -- traceMap "patternPossibilities2 = " patternPossibilities2 `seq`
        -- traceMap "patternPossibilities3 = " patternPossibilities3 `seq`
        outputVal

nSegmentsForDigit :: Int -> Int
nSegmentsForDigit 0 = 6
nSegmentsForDigit 1 = 2
nSegmentsForDigit 2 = 5
nSegmentsForDigit 3 = 5
nSegmentsForDigit 4 = 4
nSegmentsForDigit 5 = 5
nSegmentsForDigit 6 = 6
nSegmentsForDigit 7 = 3
nSegmentsForDigit 8 = 7
nSegmentsForDigit 9 = 6
nSegmentsForDigit _ = panic "unexpected digit value"


digitContainsSegments :: Digit -> Pattern -> Bool
digitContainsSegments !dig !segs
    | Just digSegs <- Map.lookup dig segmentsForDigit
    = segs `Set.isSubsetOf` digSegs
    | otherwise = panic "unexpected: digitContainsSegments"
    

segmentsForDigit :: Map Digit Pattern
segmentsForDigit =
    Map.fromList [
        (0, patternFromText "abcefg" )
      , (1, patternFromText "cf"     )
      , (2, patternFromText "acdeg"  )
      , (3, patternFromText "acdfg"  )
      , (4, patternFromText "bcdf"   )
      , (5, patternFromText "abdfg"  )
      , (6, patternFromText "abdefg" )
      , (7, patternFromText "acf"    )
      , (8, patternFromText "abcdefg")
      , (9, patternFromText "abcdfg" )
    ]

lookupDigitSegments :: Digit -> Pattern
lookupDigitSegments !d =
    fromMaybe (panic $ "failed to find pattern for digit: " <> show d) $
    Map.lookup d segmentsForDigit


{--
from 1:     b/e -> c/f  right segs
from 7:     d   -> a    top seg
from 4:     c/g -> b/d  top-left + middle
    5, 6, 9 also will have these segments (c/g signals)
        cbdgef, fgaecd, fdcge

        fdcge = 5 (since it has only 5 segs)
        cbdgef, fgaecd are 6/9 

    0, 2, 3 will only have 1
        agebfd, fecdb, fabcd

        agebfd = 0 (since it has 6 segs)
        fecdb, fabcd are 2/3

        fecdb = 3 (since it contains both right segs, from 1)
        fabcd = 2

from 0 (agebfd) and 8 (cfbegad)
    c -> d  (middle seg)

from 5 (fdcge) compared to 1 (be):
    b ->  c  (top-right)
    e ->  f  (bottom-right)

from 2 (fabcd) compared to 3 (fecdb):  since these differ only in the bottom left/right
    a -> e  (bottom-left)

from 2 (fabcd) or 3 (fecdb):
    f -> g  (bottom)        since these don't have a top-left, but do have a bottom (and there are only 2 letters left here)

by process of elimination:
    g  -> b   (top-left)

signal  seg     desc
a:      e       bottom-left
b:      c       top-right
c:      d       middle
d:      a       top
e:      f       bottom-right
f:      g       bottom
g:      b       top-left


fdgacbe cefdb cefbgd gcbe
8       3     9      4

dig     #segs
0       6
1       2   (distinct)
2       5
3       5
4       4   (distinct)
5       5
6       6
7       3   (distinct)
8       7   (distinct)
9       6

#segs   digits
5:      2, 3, 5
6:      0, 6, 9

--}

-- | Answer: 1070957
day8b :: IO ()
day8b = do
    entries :: [DisplayEntry] <- day8_readInput "data/day8a_input.txt"
    print $ sum $ day8b_compute entries

day8b_compute :: [DisplayEntry] -> [Int]
day8b_compute !entries =
    map translateDisplayEntryOutput_NEW entries
