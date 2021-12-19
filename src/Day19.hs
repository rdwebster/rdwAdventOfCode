module Day19
    ( day19a
    , day19b
    ) where

import MyPrelude
-- import Data.List.NonEmpty ((<|))
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

import Utils


-- | Answer: 362
day19a :: IO ()
day19a = do
    scannerReports :: [ScannerReport] <-
        -- day19_readInput "data/day19test_input.txt"
        day19_readInput "data/day19a_input.txt"

    putText "Scanner Reports:"
    traverse_ print scannerReports
    putText ""

    print $ day19a_compute scannerReports

day19a_compute :: [ScannerReport] -> Int
day19a_compute !scannerReports =
    let
        -- sr0 = scannerReports !! 0
        -- sr1 = scannerReports !! 1

        -- match1 = matchScannerReports sr0 sr1
        -- sr1_norm = normalizeScannerReport sr1 sr0

        allNorms = normalizeAllScannerReports scannerReports

        allBeaconCoords :: Set Coords
        allBeaconCoords =
            Set.fromList $
            concatMap beaconPositions allNorms
    in
        -- traceVal "match1 = " match1 `seq`
        -- traceVal "sr1_norm = " sr1_norm `seq`
        -- traceList "all norms = " allNorms `seq`
        -- traceSet "all beacons = " allBeaconCoords `seq`
        Set.size allBeaconCoords

type ScannerID = Int
type Coords = (Int, Int, Int)

manhattanDist :: Coords -> Coords -> Int
manhattanDist (x1,y1,z1) (x2,y2,z2) =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

rotateCoords90x :: Coords -> Coords
rotateCoords90x (!x,!y,!z) = (x,-z,y)

rotateCoords90y :: Coords -> Coords
rotateCoords90y (!x,!y,!z) = (-z,y,x)

rotateCoords90z :: Coords -> Coords
rotateCoords90z (!x,!y,!z) = (-y,x,z)

allCoordPossibilities :: Coords -> [Coords]
allCoordPossibilities !c =
    let
        c_posX = c
        c_negX = rotateCoords90z $ rotateCoords90z c

        c_posY = rotateCoords90z c
        c_negY = rotateCoords90z $ rotateCoords90z c_posY

        c_posZ = rotateCoords90y c
        c_negZ = rotateCoords90y $ rotateCoords90y c_posZ
    in
        [ -- Positive X facing
          c_posX
        , rotateCoords90x c_posX
        , rotateCoords90x $ rotateCoords90x c_posX
        , rotateCoords90x $ rotateCoords90x $ rotateCoords90x c_posX

          -- Positive X facing
        , c_negX
        , rotateCoords90x c_negX
        , rotateCoords90x $ rotateCoords90x c_negX
        , rotateCoords90x $ rotateCoords90x $ rotateCoords90x c_negX

          -- Positive Y facing
        , c_posY
        , rotateCoords90y c_posY
        , rotateCoords90y $ rotateCoords90y c_posY
        , rotateCoords90y $ rotateCoords90y $ rotateCoords90y c_posY

          -- Negative Y facing
        , c_negY
        , rotateCoords90y c_negY
        , rotateCoords90y $ rotateCoords90y c_negY
        , rotateCoords90y $ rotateCoords90y $ rotateCoords90y c_negY

          -- Positive Z facing
        , c_posZ
        , rotateCoords90z c_posZ
        , rotateCoords90z $ rotateCoords90z c_posZ
        , rotateCoords90z $ rotateCoords90z $ rotateCoords90z c_posZ

          -- Negative Z facing
        , c_negZ
        , rotateCoords90z c_negZ
        , rotateCoords90z $ rotateCoords90z c_negZ
        , rotateCoords90z $ rotateCoords90z $ rotateCoords90z c_negZ
        ]


data ScannerReport =
    ScannerReport {
        scannerID       :: !ScannerID
      , scannerPosition :: !Coords
      , beaconPositions :: ![Coords]
    }

instance Show ScannerReport where
    show ScannerReport{..} =
        Prelude.unlines $
        ("--- scanner " <> show scannerID <> " --- " <> show scannerPosition) :
        map (\(x,y,z) ->  mapJoin "," show [x,y,z]) beaconPositions


normalizeAllScannerReports :: [ScannerReport] -> [ScannerReport]
normalizeAllScannerReports [] = []
normalizeAllScannerReports (sr0 : sr_rest) =
    let
        doNormalize :: [ScannerReport] -> [ScannerReport] -> [ScannerReport]
        doNormalize [] normSRs = normSRs
        doNormalize remainingSRs normSRs =
            let
                -- Returns either the original SR (left) or the normalized one (right).
                tryToNormalizeSR :: ScannerReport -> Either ScannerReport ScannerReport
                tryToNormalizeSR !sr =
                    -- Try to normalize this SR with any of the already-normalized reports.
                    maybe (Left sr) Right $
                    asum $ normalizeScannerReport sr <$> normSRs

                (unnorms, norms) = partitionEithers $ tryToNormalizeSR <$> remainingSRs
            in
                doNormalize unnorms $ normSRs <> norms
    in
        doNormalize sr_rest [sr0]

-- | Returns the normalized version of the other scanner report if it matches
-- up significantly with the reference report.
normalizeScannerReport :: ScannerReport -> ScannerReport -> Maybe ScannerReport
normalizeScannerReport !otherSR !referenceSR =
    let
        refCoordsSet :: Set Coords
        refCoordsSet = Set.fromList $ beaconPositions referenceSR

        -- Apply all possible transformations to the beacon
        -- coords in the 'other' report.
        otherPossibleCoordSets :: [[Coords]]
        otherPossibleCoordSets =
            transpose $
            map allCoordPossibilities $
            beaconPositions otherSR

        checkPossibleCoordSet :: [Coords] -> Maybe ScannerReport
        checkPossibleCoordSet !otherCoords =
            let
                coordPairCombos :: [(Coords, Coords)]
                coordPairCombos = do
                    refC   <- beaconPositions referenceSR
                    otherC <- otherCoords
                    pure (refC, otherC)

                checkCoordsPair :: Coords -> Coords -> Maybe ScannerReport
                checkCoordsPair (refX,refY,refZ) (otherX,otherY,otherZ) =
                    let
                        xOffset = otherX - refX
                        yOffset = otherY - refY
                        zOffset = otherZ - refZ

                        offsetScannerCoords :: Coords
                        offsetScannerCoords = (-xOffset, -yOffset, -zOffset)

                        offsetOtherCoords :: [Coords]
                        offsetOtherCoords =
                            map (\(x,y,z) -> (x-xOffset,y-yOffset,z-zOffset)) otherCoords

                        numMatching :: Int
                        numMatching =
                            -- traceVal "# matching coords = " $
                            Set.size $
                            -- traceSet "matching coords = " $
                            Set.intersection refCoordsSet $
                            Set.fromList offsetOtherCoords
                    in do
                        guard $ numMatching >= 12
                        pure ScannerReport{ scannerID       = scannerID otherSR
                                          , scannerPosition = offsetScannerCoords
                                          , beaconPositions = offsetOtherCoords
                                          }
            in
                asum $ uncurry checkCoordsPair <$> coordPairCombos
    in
        -- asum $ checkPossibleCoordSet <$> otherPossibleCoordSets
        case asum $ checkPossibleCoordSet <$> otherPossibleCoordSets of
        Nothing -> Nothing
        Just n -> traceVal "Normalized SR: " (scannerID n) `seq` Just n


_matchScannerReports :: ScannerReport -> ScannerReport -> Bool
_matchScannerReports !referenceSR !otherSR =
    let
        refCoordsSet :: Set Coords
        refCoordsSet = Set.fromList $ beaconPositions referenceSR

        -- Apply all possible transformations to the beacon
        -- coords in the 'other' report.
        otherPossibleCoordSets :: [[Coords]]
        otherPossibleCoordSets =
            transpose $
            map allCoordPossibilities $
            beaconPositions otherSR

        checkPossibleCoordSet :: [Coords] -> Bool
        checkPossibleCoordSet !otherCoords =
            let
                coordPairCombos :: [(Coords, Coords)]
                coordPairCombos = do
                    refC   <- beaconPositions referenceSR
                    otherC <- otherCoords
                    pure (refC, otherC)

                checkCoordsPair :: Coords -> Coords -> Bool
                checkCoordsPair (refX,refY,refZ) (otherX,otherY,otherZ) =
                    let
                        xOffset = otherX - refX
                        yOffset = otherY - refY
                        zOffset = otherZ - refZ

                        offsetOtherCoords :: [Coords]
                        offsetOtherCoords =
                            map (\(x,y,z) -> (x-xOffset,y-yOffset,z-zOffset)) otherCoords

                        numMatching :: Int
                        numMatching =
                            -- traceVal "# matching coords = " $
                            Set.size $
                            -- traceSet "matching coords = " $
                            Set.intersection refCoordsSet $
                            Set.fromList offsetOtherCoords
                    in
                        numMatching >= 12
            in
                any (uncurry checkCoordsPair) coordPairCombos
    in
        any checkPossibleCoordSet otherPossibleCoordSets


day19_readInput :: FilePath -> IO [ScannerReport]
day19_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    return $ day19a_parseLines fileLines

day19a_parseLines :: [Text] -> [ScannerReport]
day19a_parseLines !fileLines =
    let
        scannerLineBlocks :: [[Text]]
        scannerLineBlocks =
            let
                go :: [Text] -> [[Text]]
                go [] = []
                go !remainingLines =
                    let
                        (scnrBlock, rest) = break T.null remainingLines
                    in
                        scnrBlock : go (drop 1 rest)
            in
                go fileLines

        -- Example:
        -- --- scanner 0 ---
        -- 404,-588,-901
        -- 528,-643,409
        parseScannerBlock :: [Text] -> Maybe ScannerReport
        parseScannerBlock [] = Nothing
        parseScannerBlock (headerLine : coordLines) =
            let
                scannerID :: ScannerID
                scannerID = fromMaybe (panic "invalid scanner header line") $ do
                    t1 <- T.stripPrefix "--- scanner " headerLine
                    t2 <- T.stripSuffix " ---" t1
                    readMaybe $ T.unpack t2

                -- Initially put all scanners at the origin (local to themselves).
                scannerPosition :: Coords
                scannerPosition = (0, 0, 0)

                beaconPositions :: [Coords]
                beaconPositions =
                    mapMaybe parseCoords coordLines

                parseCoords :: Text -> Maybe Coords
                parseCoords !coordsLine
                    | [xTxt,yTxt,zTxt] <- T.splitOn "," coordsLine
                    , Just x <- readMaybe $ T.unpack xTxt
                    , Just y <- readMaybe $ T.unpack yTxt
                    , Just z <- readMaybe $ T.unpack zTxt
                    = pure (x,y,z)
                    | otherwise
                    = Nothing
            in
                pure ScannerReport{..}
    in
        mapMaybe parseScannerBlock scannerLineBlocks


-- | Answer: 12204
day19b :: IO ()
day19b = do
    scannerReports :: [ScannerReport] <-
        -- day19_readInput "data/day19test_input.txt"
        day19_readInput "data/day19a_input.txt"

    print $ day19b_compute scannerReports

day19b_compute :: [ScannerReport] -> Int
day19b_compute !scannerReports =
    let
        -- sr0 = scannerReports !! 0
        -- sr1 = scannerReports !! 1
        --
        -- sr1_norm = normalizeScannerReport sr1 sr0

        allNorms =
            traceList "Normalized Scanner Reports" $
            normalizeAllScannerReports scannerReports

        scannerCoords :: [Coords]
        scannerCoords = scannerPosition <$> allNorms

        maxDist :: Int
        maxDist =
            maximum $ do
                c1 <- scannerCoords
                c2 <- scannerCoords
                pure $ manhattanDist c1 c2
    in
        -- traceVal "sr0 = " sr0 `seq`
        -- traceVal "sr1 = " sr1 `seq`
        -- traceVal "sr1_norm = " sr1_norm `seq`
        -- traceList "scannerCoords = " scannerCoords `seq`
        maxDist

