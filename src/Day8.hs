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
type Segment = Char
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

signalToSegmentMappings :: DisplayEntry -> Map Signal Segment
signalToSegmentMappings DisplayEntry{..}
    | Just onePattern   <- find (\p -> Set.size p == 2) signalPatterns
    , Just fourPattern  <- find (\p -> Set.size p == 4) signalPatterns
    , Just sevenPattern <- find (\p -> Set.size p == 3) signalPatterns
    , Just eightPattern <- find (\p -> Set.size p == 7) signalPatterns
    , [aSegSignal]      <- Set.toList $ sevenPattern `Set.difference` onePattern
    , bdSegSignals      <- fourPattern `Set.difference` onePattern
    , Just fivePattern  <- find (\p -> Set.size p == 5 && Set.isSubsetOf bdSegSignals p) signalPatterns
    , Just zeroPattern  <- find (\p -> Set.size p == 6 && not (Set.isSubsetOf bdSegSignals p)) signalPatterns
    , [dSegSignal]      <- Set.toList $ eightPattern `Set.difference` zeroPattern
    , [fSegSignal]      <- Set.toList $ onePattern `Set.intersection` fivePattern
    , [cSegSignal]      <- Set.toList $ onePattern `Set.difference` fivePattern
    , Just threePattern <- find (\p ->
                                    Set.size p == 5
                                    && not (Set.isSubsetOf bdSegSignals p)
                                    && Set.isSubsetOf onePattern p
                                ) signalPatterns
    , Just twoPattern   <- find (\p ->
                                    Set.size p == 5
                                    && not (Set.isSubsetOf bdSegSignals p)
                                    && p /= threePattern
                                ) signalPatterns
    , [eSegSignal]      <- Set.toList $ twoPattern `Set.difference` threePattern
    , [bSegSignal]      <- Set.toList $ (fivePattern `Set.difference` twoPattern) `Set.difference` onePattern
    , [gSegSignal]      <- Set.toList $ eightPattern `Set.difference` Set.fromList [aSegSignal, bSegSignal, cSegSignal, dSegSignal, eSegSignal, fSegSignal]
    =
        -- traceVal "___ onePattern = " onePattern `seq`
        -- traceVal "___ fourPattern = " fourPattern `seq`
        -- traceVal "___ sevenPattern = " sevenPattern `seq`
        -- traceVal "___ eightPattern = " eightPattern `seq`
        -- traceVal "___ aSegSignal = " aSegSignal `seq`
        -- traceVal "___ bdSegSignals = " bdSegSignals `seq`
        -- traceVal "___ zeroPattern = " zeroPattern `seq`
        -- traceVal "___ fivePattern = " fivePattern `seq`
        -- traceVal "___ dSegSignal = " dSegSignal `seq`
        -- traceVal "___ fSegSignal = " fSegSignal `seq`
        -- traceVal "___ cSegSignal = " cSegSignal `seq`
        -- traceVal "___ threePattern = " threePattern `seq`
        -- traceVal "___ twoPattern = " twoPattern `seq`
        -- traceVal "___ eSegSignal = " eSegSignal `seq`
        -- traceVal "___ bSegSignal = " bSegSignal `seq`
        -- traceVal "___ gSegSignal = " gSegSignal `seq`
        -- traceMap "signalToSegmentMappings = " $
        Map.fromList [ (aSegSignal, 'a')
                     , (bSegSignal, 'b')
                     , (cSegSignal, 'c')
                     , (dSegSignal, 'd')
                     , (eSegSignal, 'e')
                     , (fSegSignal, 'f')
                     , (gSegSignal, 'g')
                     ]
    | otherwise
    = panic "could not extract signal mappings"

translatePattern :: Map Signal Segment -> Pattern -> Digit
translatePattern !signalToSegMap !pattern =
    let
        translatedPattern :: Pattern
        translatedPattern =
            Set.map (\sig -> fromMaybe (panic "unexpected: failed to look up signal segment") $
            Map.lookup sig signalToSegMap) pattern
    in
        fromMaybe (panic $ "unexpected: failed to look up digit for signal pattern: " <> showPattern pattern <> " - translated pattern: " <> showPattern translatedPattern) $
        Map.lookup translatedPattern signalPatternToDigitMap

translateDisplayEntryOutput :: DisplayEntry -> Int
translateDisplayEntryOutput de@DisplayEntry{..} =
    let
        signalToSegMap :: Map Signal Segment
        signalToSegMap = signalToSegmentMappings de

        translatedDigits :: [Int]
        translatedDigits = translatePattern signalToSegMap <$> outputDigits
    in
        foldl' (\cur dig -> cur * 10 + dig) 0 translatedDigits


signalPatternToDigitMap :: Map Pattern Digit
signalPatternToDigitMap =
    Map.fromList [
        (patternFromText "abcefg", 0)
      , (patternFromText "cf", 1)
      , (patternFromText "acdeg", 2)
      , (patternFromText "acdfg", 3)
      , (patternFromText "bcdf", 4)
      , (patternFromText "abdfg", 5)
      , (patternFromText "abdefg", 6)
      , (patternFromText "acf", 7)
      , (patternFromText "abcdefg", 8)
      , (patternFromText "abcdfg", 9)
    ]

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
    map translateDisplayEntryOutput entries
