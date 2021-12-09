module Day9
    ( day9a
    , day9b
    ) where

import MyPrelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

import Utils


-- | Answer: 631
day9a :: IO ()
day9a = do
    heightMap :: HeightMap <- day9_readInput "data/day9a_input.txt"

    putText "HeightMap:"
    print heightMap
    putText ""

    print $ day9a_compute heightMap

day9a_compute :: HeightMap -> Int
day9a_compute !heightMap =
    let
        lows :: [(Coords, Height)]
        lows =
            traceList "lows = " $
            findLowPoints heightMap

        risks :: [Risk]
        risks = map (\l -> snd l + 1) lows
    in
        sum risks

type Height = Int
type Risk   = Int
type Coords = (Int, Int)

adjacentPoints :: Coords -> [Coords]
adjacentPoints (rowN, colN) =
    [ (rowN-1, colN)
    , (rowN+1, colN)
    , (rowN, colN-1)
    , (rowN, colN+1)
    ]

data HeightMap =
    HeightMap {
        locations :: !(Map Coords Height)
    }

instance Show HeightMap where
    show HeightMap{..} =
        let
            minX = minimum $ map snd $ Map.keys locations
            maxX = maximum $ map snd $ Map.keys locations

            minY = minimum $ map fst $ Map.keys locations
            maxY = maximum $ map fst $ Map.keys locations

            makeRow :: Int -> String
            makeRow !rowN =
                let
                    locChar :: Int -> Char
                    locChar !colN =
                        maybe '?' intToDigit $
                        Map.lookup (rowN, colN) locations
                in
                    locChar <$> [minX .. maxX]
        in
            Prelude.unlines $
            map makeRow [minY..maxY]

findLowPoints :: HeightMap -> [(Coords, Height)]
findLowPoints HeightMap{..} =
    let
        minX = minimum $ map snd $ Map.keys locations
        maxX = maximum $ map snd $ Map.keys locations

        minY = minimum $ map fst $ Map.keys locations
        maxY = maximum $ map fst $ Map.keys locations

        allPoints :: [Coords]
        allPoints = do
            rowN <- [minY..maxY]
            colN <- [minX..maxX]
            pure (rowN, colN)

        checkPoint :: Coords -> Maybe (Coords, Height)
        checkPoint coords =
            let
                coordHeight :: Height
                coordHeight =
                    fromMaybe (panic "unexpected: no height for coords") $
                    Map.lookup coords locations

                adjacentHeights :: [Height]
                adjacentHeights =
                    mapMaybe (`Map.lookup` locations) $ adjacentPoints coords
            in do
                guard $ all (> coordHeight) adjacentHeights
                pure (coords, coordHeight)
    in
        mapMaybe checkPoint allPoints

-- | Compute the number of locations in a basin containing a given low point.
sizeOfBasin :: HeightMap -> Coords -> Int
sizeOfBasin HeightMap{..} !lowPoint =
    let
        -- Starting with a single low point, expand the basin in phases
        -- to include any adjacent points to the current ones which are less then 9.
        -- Stop iterating when no new points are added.
        expandBasin :: Set Coords -> Set Coords
        expandBasin !curBasinLocs =
            let
                expandLocation :: Coords -> Set Coords
                expandLocation !loc =
                    let
                        checkAdjacent :: Coords -> Maybe Coords
                        checkAdjacent !adj = do
                            -- Check that this point isn't already in the basin locations.
                            guard $ not $ Set.member adj curBasinLocs
                            adjH <- Map.lookup adj locations
                            guard $ adjH < 9
                            pure adj
                    in
                        Set.fromList $
                        mapMaybe checkAdjacent $
                        adjacentPoints loc

                newLocs :: Set Coords
                newLocs = foldMap expandLocation curBasinLocs
            in
                if Set.null newLocs then curBasinLocs
                else expandBasin $ newLocs <> curBasinLocs
    in
        Set.size $
        expandBasin $
        Set.singleton lowPoint

day9_readInput :: FilePath -> IO HeightMap
day9_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    pure $ day9_parseHeightMap fileLines

day9_parseHeightMap :: [Text] -> HeightMap
day9_parseHeightMap !fileLines =
    let
        parseRow :: Text -> Int -> Map Coords Height
        parseRow !rowText !rowN =
            let
                parseLocation :: Char -> Int -> (Coords, Height)
                parseLocation !locChar !colN =
                    ((rowN, colN), digitToInt locChar)
            in
                Map.fromList $
                mapIndexed parseLocation $
                T.unpack rowText
    in
        HeightMap $
        mconcat $
        mapIndexed parseRow fileLines


-- | Answer: 821560
day9b :: IO ()
day9b = do
    heightMap :: HeightMap <- day9_readInput "data/day9a_input.txt"
    print $ day9b_compute heightMap

day9b_compute :: HeightMap -> Int
day9b_compute !heightMap =
    let
        lowsPoints :: [Coords]
        lowsPoints =
            map fst $
            findLowPoints heightMap

        basinSizes :: [Int]
        basinSizes =
            traceList "basinSizes = " $
            sizeOfBasin heightMap <$> lowsPoints
    in
        product $
        take 3 $
        reverse $
        sort $
        basinSizes
