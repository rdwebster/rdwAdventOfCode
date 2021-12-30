module Day25
    ( day25a
    ) where

import MyPrelude
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

import Utils


-- | Answer: 453
day25a :: IO ()
day25a = do
    initState :: SeaFloorState <-
        -- day25_readInput "data/day25test1_input.txt"     -- test
        day25_readInput "data/day25a_input.txt"         -- actual

    putText "SeaFloorState:"
    print initState
    putText ""

    putText "After one step:"
    print $ runStep initState
    putText ""

    print $ day25a_compute initState

day25a_compute :: SeaFloorState -> Int
day25a_compute !initState =
    let
        (nSteps, finalState) = runUntilStopped initState
    in
        traceVal "finalState = " finalState `seq`
        nSteps

type StepN = Int
type Pos = Int
type Coords = (Pos, Pos)    -- (x,y)

data SeaFloorState =
    SeaFloorState {
        regionWidth  :: !Int
      , regionHeight :: !Int
      , eastMovers   :: !(Set Coords)
      , southMovers  :: !(Set Coords)
    }
    deriving (Eq)

instance Show SeaFloorState where
    show SeaFloorState{..} =
        let
            rowText :: Pos -> String
            rowText !y =
                let
                    cellChar :: Pos -> Char
                    cellChar !x
                        | Set.member (x,y) eastMovers
                        = '>'
                        | Set.member (x,y) southMovers
                        = 'v'
                        | otherwise
                        = '.'
                in
                    cellChar <$> [0..(regionWidth-1)]
        in
            Prelude.unlines $ rowText <$> [0..(regionHeight-1)]

runUntilStopped :: SeaFloorState -> (StepN, SeaFloorState)
runUntilStopped !initState =
    let
        go :: StepN -> SeaFloorState -> (StepN, SeaFloorState)
        go !stepN !sfs =
            let
                nextState :: SeaFloorState
                nextState = runStep sfs
            in
                if sfs == nextState then (stepN, sfs)
                else go (stepN+1) nextState
    in
        go 1 initState

runStep :: SeaFloorState -> SeaFloorState
runStep !sfs =
    moveSouth $ moveEast sfs

-- | Moves any east-moving sea cucumber one
-- space to the east if it is empty.
moveEast :: SeaFloorState -> SeaFloorState
moveEast sfs@SeaFloorState{..} =
    let
        -- Determine which east-moving sea cucumbers
        -- have a free space to the east (and handling
        -- wrapping around at the edge).
        moveSeaCucEast :: Coords -> Maybe (Coords, Coords)
        moveSeaCucEast (x,y) =
            let
                eastCoords :: Coords
                eastCoords
                    | x+1 == regionWidth
                    = (0,y)
                    | otherwise
                    = (x+1,y)
            in do
                guard $ Set.notMember eastCoords eastMovers
                guard $ Set.notMember eastCoords southMovers
                pure ((x,y), eastCoords)

        moveFrom :: Set Coords
        moveTo   :: Set Coords
        (moveFrom, moveTo) =
            bimap Set.fromList Set.fromList $
            unzip $
            mapMaybe moveSeaCucEast $
            Set.toList eastMovers

        updatedEastMovers :: Set Coords
        updatedEastMovers =
            moveTo `Set.union`
            (eastMovers `Set.difference` moveFrom)
    in
        sfs{eastMovers = updatedEastMovers}

-- | Moves any south-moving sea cucumber one
-- space to the south if it is empty.
moveSouth :: SeaFloorState -> SeaFloorState
moveSouth sfs@SeaFloorState{..} =
    let
        -- Determine which south-moving sea cucumbers
        -- have a free space to the south (and handling
        -- wrapping around at the edge).
        moveSeaCucSouth :: Coords -> Maybe (Coords, Coords)
        moveSeaCucSouth (x,y) =
            let
                southCoords :: Coords
                southCoords
                    | y+1 == regionHeight
                    = (x,0)
                    | otherwise
                    = (x,y+1)
            in do
                guard $ Set.notMember southCoords eastMovers
                guard $ Set.notMember southCoords southMovers
                pure ((x,y), southCoords)

        moveFrom :: Set Coords
        moveTo   :: Set Coords
        (moveFrom, moveTo) =
            bimap Set.fromList Set.fromList $
            unzip $
            mapMaybe moveSeaCucSouth $
            Set.toList southMovers

        updatedSouthMovers :: Set Coords
        updatedSouthMovers =
            moveTo `Set.union`
            (southMovers `Set.difference` moveFrom)
    in
        sfs{southMovers = updatedSouthMovers}

day25_readInput :: FilePath -> IO SeaFloorState
day25_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    return $ day25a_parseLines fileLines

day25a_parseLines :: [Text] -> SeaFloorState
day25a_parseLines !sfLines =
    let
        regionWidth  :: Int
        regionWidth = T.length $ unsafeHead sfLines

        regionHeight  :: Int
        regionHeight = length sfLines

        coordsForSymbol :: Char -> Set Coords
        coordsForSymbol !symbol =
            let
                coordsFromLine :: Text -> Int -> Set Coords
                coordsFromLine !lineTxt !y =
                    mconcat $
                    zipWith (\ch x ->
                                if ch == symbol then Set.singleton (x,y) else Set.empty
                            ) (T.unpack lineTxt) [0..]
            in
                mconcat $
                zipWith coordsFromLine sfLines [0..]

        eastMovers  :: Set Coords
        eastMovers = coordsForSymbol '>'

        southMovers  :: Set Coords
        southMovers = coordsForSymbol 'v'
    in
        SeaFloorState{..}
