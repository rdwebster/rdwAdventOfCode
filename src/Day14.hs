module Day14
    ( day14a
    , day14b
    ) where

import MyPrelude
import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

-- import Utils


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

        elemCounts :: Map Elem Int
        elemCounts =
            Map.fromListWith (+) $
            map (\e -> (e, 1)) $
            T.unpack template10

        elemCountsSorted :: [Int]
        elemCountsSorted =
            sort $ Map.elems elemCounts

        leastCommonCount :: Int
        leastCommonCount = unsafeHead elemCountsSorted

        mostCommonCount :: Int
        mostCommonCount = unsafeLast elemCountsSorted
    in
        mostCommonCount - leastCommonCount


type PolymerTemplate = Text

type Elem = Char

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

runPairInsertionStep :: Map (Elem, Elem) Elem -> PolymerTemplate -> PolymerTemplate
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


-- | Answer: ???
day14b :: IO ()
day14b = do
    (template, insertionRules) :: (PolymerTemplate, [PairInsertionRule]) <- day14_readInput "data/day14a_input.txt"
    print $ day14b_compute template insertionRules

day14b_compute :: PolymerTemplate -> [PairInsertionRule] -> Int
day14b_compute !template !insertionRules =
    panic "xxx"
