module Day3
    ( day3a
    , day3b
    ) where

import MyPrelude
import qualified Data.Text as T
import qualified Prelude

import Utils


-- | Answer: 3374136
day3a :: IO ()
day3a = do
    diagNums :: [DiagnosticNum] <- day3_readInput "data/day3a_input.txt"

    putText "Diagnostic Numbers:"
    traverse_ print diagNums
    putText ""

    print $ day3a_compute diagNums

day3a_compute :: [DiagnosticNum] -> Int
day3a_compute !diagNums =
    let
        (gamma, epsilon) = traceVal "powerConsumptionRates = " $ powerConsumptionRates diagNums
    in
        diagnosticNumToInt gamma * diagnosticNumToInt epsilon

type Pos = Int

newtype DiagnosticNum =
    DiagnosticNum {
        diagNumBits :: [Bool]
    }

instance Show DiagnosticNum where
    show (DiagnosticNum digs) =
        map (\d -> if d then '1' else '0') digs

invertDiagnosticNum :: DiagnosticNum -> DiagnosticNum
invertDiagnosticNum (DiagnosticNum bits) =
    DiagnosticNum $ not <$> bits

diagnosticNumToInt :: DiagnosticNum -> Int
diagnosticNumToInt (DiagnosticNum bits) =
    foldl' (\curInt bitSet -> curInt * 2 + (if bitSet then 1 else 0)) 0 bits

nthBit :: DiagnosticNum -> Pos -> Bool
nthBit (DiagnosticNum bits) pos = bits !! pos


-- | Computes the power consumption rates (gamma and epsilon).
powerConsumptionRates :: [DiagnosticNum] -> (DiagnosticNum, DiagnosticNum)
powerConsumptionRates nums =
    let
        bitsByPosition :: [[Bool]]
        bitsByPosition =
            -- traceList "bitsByPosition = " $
            transpose $
            map diagNumBits nums

        gammaFromBits :: [Bool] -> Bool
        gammaFromBits bits =
            -- traceWithArg "gammaFromBits " bits $
            compareSetToUnsetBits bits /= LT

        gamma :: DiagnosticNum
        gamma = DiagnosticNum $ gammaFromBits <$> bitsByPosition

        epsilon :: DiagnosticNum
        epsilon = invertDiagnosticNum gamma
    in
        (gamma, epsilon)
    

day3_readInput :: FilePath -> IO [DiagnosticNum]
day3_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    return $ map day3a_parseLine fileLines

day3a_parseLine :: Text -> DiagnosticNum
day3a_parseLine !txt =
    let
        readChar :: Char -> Bool
        readChar '0' = False
        readChar '1' = True
        readChar c = panic $ "unexpected char: " <> T.singleton c
    in
        DiagnosticNum $
        map readChar $
        T.unpack txt


-- | Answer: 4432698
day3b :: IO ()
day3b = do
    diagNums :: [DiagnosticNum] <- day3_readInput "data/day3a_input.txt"
    print $ day3b_compute diagNums

day3b_compute :: [DiagnosticNum] -> Int
day3b_compute !diagNums =
    let
        oxygen = diagnosticNumToInt $ findOxygenGenRating diagNums
        co2    = diagnosticNumToInt $ findCO2ScrubberRating diagNums
    in
        oxygen * co2

findOxygenGenRating :: [DiagnosticNum] -> DiagnosticNum
findOxygenGenRating allDiagNums =
    let
        search :: [DiagnosticNum] -> Pos -> DiagnosticNum
        search [] _pos = panic "unexpected - empty list of numbers"
        search [singleNum] _pos = singleNum
        search !curNums !pos =
            let
                mostCommonInPos :: Bool
                mostCommonInPos =
                    traceWithArg "mostCommonInPos" pos $
                    compareSetToUnsetBits (map (\n -> nthBit n pos) curNums) /= LT

                filteredNums :: [DiagnosticNum]
                filteredNums =
                    filter (\num -> nthBit num pos == mostCommonInPos) curNums
            in
                search filteredNums (pos + 1)
    in
        traceVal "findOxygenGenRating = " $
        search allDiagNums 0


findCO2ScrubberRating :: [DiagnosticNum] -> DiagnosticNum
findCO2ScrubberRating allDiagNums =
    let
        search :: [DiagnosticNum] -> Pos -> DiagnosticNum
        search [] _pos = panic "unexpected - empty list of numbers"
        search [singleNum] _pos = singleNum
        search !curNums !pos =
            let
                leastCommonInPos :: Bool
                leastCommonInPos =
                    traceWithArg "leastCommonInPos" pos $
                    compareSetToUnsetBits (map (\n -> nthBit n pos) curNums) == LT

                filteredNums :: [DiagnosticNum]
                filteredNums =
                    filter (\num -> nthBit num pos == leastCommonInPos) curNums
            in
                search filteredNums (pos + 1)
    in
        traceVal "findCO2ScrubberRating = " $
        search allDiagNums 0


compareSetToUnsetBits :: [Bool] -> Ordering
compareSetToUnsetBits !bits =
    let
        checkBits :: Int -> Int -> [Bool] -> Ordering
        checkBits !nSet !nUnset [] = nSet `compare` nUnset
        checkBits !nSet !nUnset (True : rest) =
            checkBits (nSet + 1) nUnset rest
        checkBits !nSet !nUnset (False : rest) =
            checkBits nSet (nUnset + 1) rest
    in
        checkBits 0 0 bits
