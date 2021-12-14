module Day14
    ( day14a
    , day14b
    ) where

import MyPrelude
import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

import Utils


-- | Answer: 2851
day14a :: IO ()
day14a = do
    (template, insertionRules) :: (PolymerTemplate, [PairInsertionRule]) <- day14_readInput "data/day14a_input.txt"

    putText "Polymer Template:"
    putText template
    putText ""
    putText "Pair Insertion Rules:"
    traverse_ print insertionRules
    putText ""

    print $ day14a_compute template insertionRules

day14a_compute :: PolymerTemplate -> [PairInsertionRule] -> Int
day14a_compute !template !insertionRules =
    let
        insertionRulesMap :: Map (Elem, Elem) Elem
        insertionRulesMap =
            Map.fromList $
            map (\PairInsertionRule{..} -> ((elem1, elem2), insertElem)) insertionRules

        template10 :: PolymerTemplate
        template10 =
            unsafeHead $
            drop 10 $
            iterate (runPairInsertionStep insertionRulesMap) template

        elemCountsSorted :: [Count]
        elemCountsSorted =
            sort $ Map.elems $ templateElemCounts template10

        leastCommonCount :: Count
        leastCommonCount = unsafeHead elemCountsSorted

        mostCommonCount :: Count
        mostCommonCount = unsafeLast elemCountsSorted
    in
        mostCommonCount - leastCommonCount


type PolymerTemplate = Text
type Elem = Char
type ElemPair = (Elem, Elem)
type Count = Int

templateElemCounts :: PolymerTemplate -> Map Elem Count
templateElemCounts !template =
    Map.fromListWith (+) $
    map (\e -> (e, 1)) $
    T.unpack template

elemPairsInTemplate :: PolymerTemplate -> [ElemPair]
elemPairsInTemplate !template =
    let
        extractPairs :: [Elem] -> [ElemPair]
        extractPairs (e1 : e2 : rest) = (e1, e2) : extractPairs (e2 : rest)
        extractPairs _ = []
    in
        extractPairs $ T.unpack template


data PairInsertionRule =
    PairInsertionRule {
        elem1      :: !Elem
      , elem2      :: !Elem
      , insertElem :: !Elem
    }

instance Show PairInsertionRule where
    show PairInsertionRule{..} =
        mconcat [
            [elem1, elem2]
          , " -> "
          , [insertElem]
        ]

runPairInsertionStep :: Map ElemPair Elem -> PolymerTemplate -> PolymerTemplate
runPairInsertionStep insertionRulesMap template =
    let
        run :: String -> String
        run (e1 : e2 : rest) = 
            case Map.lookup (e1, e2) insertionRulesMap of
            Nothing  ->
                -- e1 : run (e2 : rest)
                panic "no insertion found"
            Just ins -> e1 : ins : run (e2 : rest)
        run other = other
    in
        T.pack $ run $ T.unpack template

-- | Instead of building the actual polymer template string, just compute show the
-- counts of each element pair change after one step.
-- Each pair will become 2 pairs, so just carry over the counts to these.
runPairInsertionStepB :: Map ElemPair Elem -> Map ElemPair Count -> Map ElemPair Count
runPairInsertionStepB insertionRulesMap elemPairCounts =
    let
        iterateElemPair :: (ElemPair, Count) -> [(ElemPair, Count)]
        iterateElemPair (pr@(e1, e2), pairCount)
            | Just eInsert <- Map.lookup pr insertionRulesMap
            = [ ((e1, eInsert), pairCount)
              , ((eInsert, e2), pairCount)
              ]
            | otherwise
            = panic "no insertion found for elem pair"
    in
        Map.fromListWith (+) $
        concatMap iterateElemPair $
        Map.toList elemPairCounts


day14_readInput :: FilePath -> IO (PolymerTemplate, [PairInsertionRule])
day14_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    let template = unsafeHead fileLines
    let insertionRules = mapMaybe day14a_parseLine_PairInsertionRule $ drop 1 fileLines
    pure (template, insertionRules)

day14a_parseLine_PairInsertionRule :: Text -> Maybe PairInsertionRule
day14a_parseLine_PairInsertionRule !txt
    | [elemsTxt, insertTxt]   <- T.splitOn " -> " txt
    , [elem1, elem2] <- T.unpack elemsTxt
    , [insertElem] <- T.unpack insertTxt
    = Just PairInsertionRule{..}
    | otherwise
    = Nothing


-- | Answer: 10002813279337
day14b :: IO ()
day14b = do
    (template, insertionRules) :: (PolymerTemplate, [PairInsertionRule]) <- day14_readInput "data/day14a_input.txt"
    print $ day14b_compute template insertionRules

day14b_compute :: PolymerTemplate -> [PairInsertionRule] -> Int
day14b_compute !template !insertionRules =
    let
        iterationN :: Int
        iterationN = 40

        lastElem :: Elem
        lastElem = unsafeLast $ T.unpack template

        insertionRulesMap :: Map (Elem, Elem) Elem
        insertionRulesMap =
            Map.fromList $
            map (\PairInsertionRule{..} -> ((elem1, elem2), insertElem)) insertionRules

        initialPairCounts :: Map ElemPair Count
        initialPairCounts =
            Map.fromListWith (+) $
            map (, 1) $
            elemPairsInTemplate template

        pairCountsN :: Map ElemPair Count
        pairCountsN =
            unsafeHead $
            drop iterationN $
            iterate (runPairInsertionStepB insertionRulesMap) initialPairCounts

        elemCountsN :: Map Elem Count
        elemCountsN =
            pairCountsToElemCounts lastElem pairCountsN

        elemCountsSorted :: [Count]
        elemCountsSorted =
            sort $ Map.elems elemCountsN

        leastCommonCount :: Count
        leastCommonCount = unsafeHead elemCountsSorted

        mostCommonCount :: Count
        mostCommonCount = unsafeLast elemCountsSorted
    in
        traceMap "pairCountsN = " pairCountsN `seq`
        traceMap "elemCounts = " elemCountsN `seq`
        mostCommonCount - leastCommonCount


pairCountsToElemCounts :: Elem -> Map ElemPair Count -> Map Elem Count
pairCountsToElemCounts !lastElem !pairCounts =
    -- The pairs overlap, so just use the first elem in each pair.
    -- Then add one count for the last elem in the pattern (which will not change
    -- from one iteration to the next).
    Map.fromListWith (+) $
    (lastElem, 1) : (map (\((e1, _e2), c) -> (e1, c)) $ Map.toList pairCounts)

