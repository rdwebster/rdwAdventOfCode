module Day7
    ( day7a
    , day7b
    ) where

import MyPrelude
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
-- import qualified Prelude

-- import Utils


-- | Answer: 359648
day7a :: IO ()
day7a = do
    crabPositions :: [Pos] <- day7_readInput "data/day7a_input.txt"

    putText "Crab Positions:"
    print crabPositions
    putText ""

    print $ day7a_compute crabPositions

day7a_compute :: [Pos] -> Fuel
day7a_compute !crabPositions =
    let
        totalFuelForHPosition :: Pos -> Fuel
        totalFuelForHPosition !hPos =
            let
                fuelForCrab :: Pos -> Fuel
                fuelForCrab !crabPos =
                    abs $ crabPos - hPos
            in
                sum $
                map fuelForCrab crabPositions
    in
        minimum $
        map totalFuelForHPosition $
        [minimum crabPositions .. maximum crabPositions]

type Pos = Int
type Fuel = Int

day7_readInput :: FilePath -> IO [Pos]
day7_readInput !fileName = do
    fileTxt <- readFile fileName
    return $ day7a_parseLine fileTxt

day7a_parseLine :: Text -> [Pos]
day7a_parseLine !txt =
    mapMaybe (readMaybe . T.unpack) $
    T.splitOn "," txt


-- | Answer: 100727924
day7b :: IO ()
day7b = do
    crabPositions :: [Pos] <- day7_readInput "data/day7a_input.txt"
    print $ day7b_compute crabPositions

day7b_compute :: [Pos] -> Fuel
day7b_compute !crabPositions =
    let
        -- Generate a Vector where each item stores the fuel cost to move
        -- the index number of steps from the baseline.
        fuelCostsV :: Vector Fuel
        fuelCostsV =
            V.unfoldrN (maximum crabPositions - minimum crabPositions + 1)
                       (\(fuel,pos) -> pure (fuel+pos, (fuel+pos, pos+1)))
                       (0, 0)

        totalFuelForHPosition :: Pos -> Fuel
        totalFuelForHPosition !hPos =
            let
                -- Fuel increases per step.
                fuelForCrab :: Pos -> Fuel
                fuelForCrab !crabPos =
                    let
                        nSteps :: Int
                        nSteps = abs $ crabPos - hPos
                    in
                        fuelCostsV V.! nSteps
            in
                sum $
                map fuelForCrab crabPositions
    in
        minimum $
        map totalFuelForHPosition $
        [minimum crabPositions .. maximum crabPositions]
