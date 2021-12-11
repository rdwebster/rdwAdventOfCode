module Day11
    ( day11a
    , day11b
    ) where

import MyPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

import Utils


-- | Answer: 1732
day11a :: IO ()
day11a = do
    grid :: OctopusGrid <- day11_readInput "data/day11a_input.txt"

    putText "Initial Octopus Grid:"
    print grid
    putText ""

    print $ day11a_compute grid

day11a_compute :: OctopusGrid -> Int
day11a_compute !grid =
    run100Steps 0 0 grid
    

type Coords = (Int, Int)
type PowerLevel = Int

adjacentPositions :: Coords -> Set Coords
adjacentPositions (rowN, colN) =
    Set.fromList $ do
        r <- [rowN-1 .. rowN+1]
        c <- [colN-1 .. colN+1]
        guard $ (r,c) /= (rowN, colN)
        guard $ r >= 0 && r < 10
        guard $ c >= 0 && c < 10
        pure (r, c)
    
newtype OctopusGrid =
    OctopusGrid {
        octopusPowerLevels :: Map Coords PowerLevel
    }

instance Show OctopusGrid where
    show OctopusGrid{..} =
        let
            cellStr :: Int -> Int -> Char
            cellStr !rowN !colN =
                maybe '?' intToDigit $
                Map.lookup (rowN, colN) octopusPowerLevels

            rowStr :: Int -> String
            rowStr !rowN =
                cellStr rowN <$> [0..9]
        in
            Prelude.unlines $
            rowStr <$> [0..9]

-- | Advance the state of the octopus grid by one step.
stepGrid :: OctopusGrid -> OctopusGrid
stepGrid OctopusGrid{..} =
    let
        -- First, increase the power level of each octopus by 1.
        levels' :: Map Coords PowerLevel
        levels' = (+ 1) <$> octopusPowerLevels
    in
        OctopusGrid $ handleFlashes levels'

run100Steps :: Int -> Int -> OctopusGrid -> Int
run100Steps 101 !totalFlashes _grid = totalFlashes
run100Steps !stepN !totalFlashes !grid =
    let
        newGrid :: OctopusGrid
        newGrid =
            traceVal ("newGrid(" <> show (stepN + 1) <> "):\n") $
            stepGrid grid

        newFlashes :: Int
        newFlashes =
            length $
            filter (== 0) $
            Map.elems $
            octopusPowerLevels grid
    in
        run100Steps (stepN + 1) (totalFlashes + newFlashes) newGrid

nStepsUntilAllFlash :: Int -> OctopusGrid -> Int
nStepsUntilAllFlash !stepN grid@OctopusGrid{..}
    | all (== 0) (Map.elems octopusPowerLevels) = stepN
    | otherwise =
        nStepsUntilAllFlash (stepN + 1) (stepGrid grid)

-- | Flash any octopus with a level > 9.
-- Update any adjacent positions.
-- Set flashed positions to 0.
handleFlashes :: Map Coords PowerLevel -> Map Coords PowerLevel
handleFlashes !origLevels =
    let
        -- Find all positions where the power level is now > 9.
        flashCoords :: Set Coords
        flashCoords =
            Map.keysSet $
            Map.filter (> 9) origLevels

        -- Set the flashed coords to zero.
        levelsAfterFlashes :: Map Coords PowerLevel
        levelsAfterFlashes =
            fmap (\l -> if l > 9 then 0 else l) origLevels

        -- Update the adjacent positions for each flash (one by one).
        levelsAfterAdjacentUpdates :: Map Coords PowerLevel
        levelsAfterAdjacentUpdates =
            foldl' (\curMap flashCoord ->
                        let
                            adjToFlash :: Set Coords
                            adjToFlash = adjacentPositions flashCoord
                        in
                            foldl' (\cur c -> Map.update (\lvl -> Just $ if lvl == 0 then 0 else lvl + 1) c cur) curMap $ Set.toList adjToFlash
                   ) levelsAfterFlashes $
            Set.toList flashCoords
    in
        if Set.null flashCoords then origLevels
        else handleFlashes levelsAfterAdjacentUpdates


day11_readInput :: FilePath -> IO OctopusGrid
day11_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    return $ OctopusGrid $ mconcat $ zipWith day11a_parseLine fileLines [0..]

day11a_parseLine :: Text -> Int -> Map Coords PowerLevel
day11a_parseLine !txt !rowN =
    Map.fromList $
    zipWith (\colN c -> ((rowN, colN), digitToInt c)) [0..] $
    T.unpack txt


-- | Answer: 290
day11b :: IO ()
day11b = do
    grid :: OctopusGrid <- day11_readInput "data/day11a_input.txt"
    print $ day11b_compute grid

day11b_compute :: OctopusGrid -> Int
day11b_compute !grid =
    nStepsUntilAllFlash 0 grid

