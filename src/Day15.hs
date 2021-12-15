module Day15
    ( day15a
    , day15b
    ) where

import MyPrelude
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

-- import Utils


-- | Answer: 739
day15a :: IO ()
day15a = do
    grid :: Grid <- day15_readInput "data/day15a_input.txt"

    putText "Initial Grid:"
    print grid
    putText ""

    print $ day15a_compute grid

day15a_compute :: Grid -> Int
day15a_compute !grid =
    let
        lowestRiskPaths :: Map Coords Path
        _paths :: [Path]
        (lowestRiskPaths, _paths) = pathsThroughGrid2 grid
    in
        -- traceMap "lowestRiskPaths = " lowestRiskPaths `seq`
        -- traceList "paths = " paths `seq`
        maybe (panic "unexpected - no risk found for end coords") pathRisk $
        Map.lookup (gridEndCoords grid) lowestRiskPaths


type Coords = (Int, Int)
type RiskLevel = Int

data Path =
    Path {
        pathCoords :: !(NonEmpty Coords)
      , pathRisk   :: !RiskLevel
    }
    deriving (Show)

pathHead :: Path -> Coords
pathHead = NonEmpty.head . pathCoords

data Grid =
    Grid {
        riskLevelFn :: Coords -> RiskLevel
      , width       :: !Int
      , height      :: !Int
    }

makeGrid :: Int -> Int -> Map Coords RiskLevel -> Grid
makeGrid !width !height !riskLevels =
    let
        riskLevelFn :: Coords -> RiskLevel
        riskLevelFn !coords =
            fromMaybe (panic "unexpected - no risk level for coords") $
            Map.lookup coords riskLevels
    in
        Grid{..}

gridEndCoords :: Grid -> Coords
gridEndCoords Grid{..} = (width-1, height-1)

areCoordsInGrid :: Grid -> Coords -> Bool
areCoordsInGrid Grid{..} (x, y) =
    x >= 0
    && y >= 0
    && x < width
    && y < height

enlargeGrid :: Grid -> Int -> Grid
enlargeGrid Grid{..} !scaleFactor =
    let
        newRiskLevelFn :: Coords -> RiskLevel
        newRiskLevelFn (!x, !y) =
            let
                xPanel :: Int
                xOrig  :: Int
                (xPanel, xOrig) = x `divMod` width

                yPanel :: Int
                yOrig  :: Int
                (yPanel, yOrig) = y `divMod` height

                origRisk :: RiskLevel
                origRisk = riskLevelFn (xOrig, yOrig)

                adjustedRisk :: RiskLevel
                adjustedRisk = origRisk + xPanel + yPanel
            in
                if adjustedRisk > 9 then adjustedRisk - 9 else adjustedRisk
    in
        Grid{
            riskLevelFn = newRiskLevelFn
          , width       = width * scaleFactor
          , height      = height * scaleFactor
        }

instance Show Grid where
    show Grid{..} =
        let
            cellStr :: Int -> Int -> Char
            cellStr !y !x =
                intToDigit $ riskLevelFn (x, y)

            rowStr :: Int -> String
            rowStr !y =
                cellStr y <$> [0..(width-1)]
        in
            Prelude.unlines $
            rowStr <$> [0..(height-1)]

pathsThroughGrid2 :: Grid -> (Map Coords Path, [Path])
pathsThroughGrid2 grid@Grid{..} =
    let
        startCoord :: Coords
        startCoord = (0, 0)

        endCoord :: Coords
        endCoord = gridEndCoords grid

        startingPath :: Path
        startingPath = Path (startCoord :| []) 0

        findPaths :: Map Coords Path -> [Path] -> (Map Coords Path, [Path])
        findPaths !lowestRiskPaths !curPaths =
            -- traceList "findPaths :: curPaths = " curPaths `seq`
            -- traceList "findPaths = " $
            if all isPathAtEnd curPaths then (lowestRiskPaths, curPaths)
            else
                uncurry findPaths $
                foldl' (\(curLowestRisk, curExtendedPaths) path ->
                            (<> curExtendedPaths) <$> extendPath curLowestRisk path
                       )
                       (lowestRiskPaths, [])
                       curPaths

        isPathAtEnd :: Path -> Bool
        isPathAtEnd path = pathHead path == endCoord

        extendPath :: Map Coords Path -> Path -> (Map Coords Path, [Path])
        extendPath !lowestRiskPaths path | isPathAtEnd path = (lowestRiskPaths, [path])
        extendPath !lowestRiskPaths path =
            let
                (headX, headY) = pathHead path
                adjacentCoords :: [Coords]
                adjacentCoords =
                    filter (areCoordsInGrid grid) $
                    [ (headX - 1, headY)
                    , (headX + 1, headY)
                    , (headX, headY - 1)
                    , (headX, headY + 1)
                    ]

                hasVisitedCoords :: Coords -> Bool
                hasVisitedCoords !coords =
                    coords `elem` pathCoords path

                nonVisitedCoords :: [Coords]
                nonVisitedCoords =
                    filter (not . hasVisitedCoords) adjacentCoords

                makeExtendedPath :: Path -> Coords -> Path
                makeExtendedPath Path{..} !newHead =
                    let
                        newRisk :: RiskLevel
                        newRisk = riskLevelFn newHead
                    in
                        Path{
                            pathCoords = newHead <| pathCoords
                          , pathRisk   = pathRisk + newRisk
                        }

                newPaths :: [Path]
                newPaths = map (makeExtendedPath path) nonVisitedCoords

                isBestPath :: Path -> Bool
                isBestPath !p =
                    case Map.lookup (pathHead p) lowestRiskPaths of
                    Nothing -> True
                    Just best -> pathRisk p < pathRisk best

                -- Only keep possible new paths if they are lower risk than any existing known path to the same coords.
                bestPaths :: [Path]
                bestPaths =
                    filter isBestPath newPaths

                newLowestRiskPaths :: Map Coords Path
                newLowestRiskPaths =
                    (Map.fromList $ map (\p -> (pathHead p, p)) bestPaths)
                    `Map.union`
                    lowestRiskPaths
            in
                (newLowestRiskPaths, bestPaths)
    in
        findPaths Map.empty [startingPath]


day15_readInput :: FilePath -> IO Grid
day15_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
        width  :: Int = maybe 0 T.length $ head fileLines
        height :: Int = length fileLines
    return $ makeGrid width height $ mconcat $ zipWith day15a_parseLine fileLines [0..]

day15a_parseLine :: Text -> Int -> Map Coords RiskLevel
day15a_parseLine !txt !y =
    Map.fromList $
    zipWith (\x c -> ((x, y), digitToInt c)) [0..] $
    T.unpack txt


-- | Answer: 3040
day15b :: IO ()
day15b = do
    grid :: Grid <- day15_readInput "data/day15a_input.txt"

    putText "Initial Grid:"
    print grid
    putText ""

    let grid5by5 :: Grid = enlargeGrid grid 5

    putText "Enlarged Grid:"
    print grid5by5
    putText ""

    print $ day15a_compute grid5by5

