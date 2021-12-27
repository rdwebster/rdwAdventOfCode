module Day23
    ( day23a
    , day23b
    ) where

import MyPrelude
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, writeTVar)
-- import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
-- import qualified Text.Megaparsec as P
-- import qualified Text.Megaparsec.Char as PC
-- import qualified Text.Megaparsec.Char.Lexer as PL
import qualified Prelude

import Utils

-- | Answer: 11332
day23a :: IO ()
day23a = do
    let initRoomAmphs =
            -- [[AmphB, AmphA], [AmphC, AmphD], [AmphB, AmphC], [AmphD, AmphA]]  -- test
            [[AmphA, AmphB], [AmphC, AmphA], [AmphB, AmphD], [AmphD, AmphC]]  -- actual

    let initialState :: BurrowState
        initialState = makeInitialBurrowState initRoomAmphs

    putText "Starting State:"
    print initialState

    let nextStates = nextPossibleBurrowStates initialState
    putText "Next States:"
    traverse_ print nextStates    

    res :: Int <- day23a_compute initialState
    print res

day23a_compute :: BurrowState -> IO Int
day23a_compute !initialState = do
    minMoveCostVar <- newTVarIO maxBound
    allFinalStates :: [BurrowState] <- runSteps minMoveCostVar initialState
    pure $ minimum $ totalMoveCost <$> allFinalStates


type StepCount  = Int
type MoveCost   = Int

data Room =
    RoomA
  | RoomB
  | RoomC
  | RoomD
    deriving (Enum, Bounded, Eq, Ord, Show)

data HallSpot =
    HallSpot1
  | HallSpot2
  | HallSpot3
  | HallSpot4
  | HallSpot5
  | HallSpot6
  | HallSpot7
    deriving (Enum, Bounded, Eq, Ord, Show)

data AmphipodType =
    AmphA
  | AmphB
  | AmphC
  | AmphD
    deriving (Enum, Bounded, Eq, Show)

amphMoveCost :: AmphipodType -> MoveCost
amphMoveCost AmphA = 1
amphMoveCost AmphB = 10
amphMoveCost AmphC = 100
amphMoveCost AmphD = 1000

amphFinalRoom :: AmphipodType -> Room
amphFinalRoom AmphA = RoomA
amphFinalRoom AmphB = RoomB
amphFinalRoom AmphC = RoomC
amphFinalRoom AmphD = RoomD

roomFinalAmph :: Room -> AmphipodType
roomFinalAmph RoomA = AmphA
roomFinalAmph RoomB = AmphB
roomFinalAmph RoomC = AmphC
roomFinalAmph RoomD = AmphD

data RoomState =
    RoomState {
        room           :: !Room
      , roomAmphipods  :: !(Vector (Maybe AmphipodType))        -- top to bottom
      -- , topAmphipod    :: !(Maybe AmphipodType)
      -- , bottomAmphipod :: !(Maybe AmphipodType)
    }
    deriving (Show)

nAmphipodsInRoom :: RoomState -> Int
nAmphipodsInRoom RoomState{..} =
    length $
    filter isJust $
    V.toList roomAmphipods

isRoomFinished :: RoomState -> Bool
isRoomFinished RoomState{..} =
    all (\maybeAmph -> (amphFinalRoom <$> maybeAmph) == Just room) roomAmphipods

canMoveAmphToRoom :: AmphipodType -> RoomState -> Bool
canMoveAmphToRoom !amphType RoomState{..} =
    all (\maybeAmph -> maybe True (== amphType) maybeAmph) roomAmphipods
    && any isNothing roomAmphipods

openRoomPosition :: RoomState -> Maybe Int
openRoomPosition rs@RoomState{..}
    | nAmphsAlreadyInRoom :: Int <- nAmphipodsInRoom rs
    , nPositions <- V.length roomAmphipods
    = if nAmphsAlreadyInRoom < nPositions
      then Just $ nPositions - nAmphsAlreadyInRoom - 1
      else Nothing

moveAmphToRoom :: RoomState -> RoomState
moveAmphToRoom rs@RoomState{..}
    | Just newAmphPos <- openRoomPosition rs
    = rs{ roomAmphipods = V.update roomAmphipods $
                          V.singleton (newAmphPos, Just $ roomFinalAmph room)
        }
    | otherwise
    = panic "unexpected: moveAmphToRoom"

moveAmphFromRoom :: RoomState -> RoomState
moveAmphFromRoom rs@RoomState{..}
    | nAmphsAlreadyInRoom :: Int <- nAmphipodsInRoom rs
    , nAmphsAlreadyInRoom > 0
    , nPositions <- V.length roomAmphipods
    , clearAmphPos <- nPositions - nAmphsAlreadyInRoom
    = rs{ roomAmphipods = V.update roomAmphipods $
                          V.singleton (clearAmphPos, Nothing)
        }
    | otherwise
    = panic "unexpected: moveAmphFromRoom"

data HallSpotState =
    HallSpotState {
        spotID   :: !HallSpot
      , amphipod :: !(Maybe AmphipodType)
    }
    deriving (Show)

data BurrowState =
    BurrowState {
        hallwayState  :: !(Map HallSpot HallSpotState)
      , roomsState    :: !(Map Room RoomState)
      , totalMoveCost :: !MoveCost
    }

instance Show BurrowState where
    show st@BurrowState{..} =
        let
            amphChar :: AmphipodType -> Char
            amphChar AmphA = 'A'
            amphChar AmphB = 'B'
            amphChar AmphC = 'C'
            amphChar AmphD = 'D'

            hallSpotChar :: HallSpot -> Char
            hallSpotChar !hallSpot =
                maybe '.' amphChar $ hallwaySpotOccupant st hallSpot

            nRoomPositions :: Int
            nRoomPositions
                | Just RoomState{..} <- Map.lookup RoomA roomsState
                = V.length roomAmphipods
                | otherwise
                = panic "unexpected: nRoomPositions"

            roomCharAtPos :: Room -> Int -> Char
            roomCharAtPos !roomToCheck !roomPos
                | Just RoomState{..} <- Map.lookup roomToCheck roomsState
                , maybeAmph <- roomAmphipods V.! roomPos
                = maybe '.' amphChar maybeAmph
                | otherwise
                = '?'

            h1 = hallSpotChar HallSpot1
            h2 = hallSpotChar HallSpot2
            h3 = hallSpotChar HallSpot3
            h4 = hallSpotChar HallSpot4
            h5 = hallSpotChar HallSpot5
            h6 = hallSpotChar HallSpot6
            h7 = hallSpotChar HallSpot7

            -- (rAtop, rAbtm) = roomChars RoomA
            -- (rBtop, rBbtm) = roomChars RoomB
            -- (rCtop, rCbtm) = roomChars RoomC
            -- (rDtop, rDbtm) = roomChars RoomD

            roomRows :: [String]
            roomRows =
                map (\posN ->
                        let
                            rA = roomCharAtPos RoomA posN
                            rB = roomCharAtPos RoomB posN
                            rC = roomCharAtPos RoomC posN
                            rD = roomCharAtPos RoomD posN
                        in
                            ['#', '#', '#', rA, '#', rB, '#', rC, '#', rD, '#', '#', '#']
                    ) $
                [0 .. (nRoomPositions - 1)]
                    
                    
        in
            Prelude.unlines $
              [ "BurrowState: cost = " <> show totalMoveCost
              , "#############"
              , ['#', h1, h2, '.', h3, '.', h4, '.', h5, '.', h6, h7, '#']
              -- , ['#', '#', '#', rAtop, '#', rBtop, '#', rCtop, '#', rDtop, '#', '#', '#']
              -- , [' ', ' ', '#', rAbtm, '#', rBbtm, '#', rCbtm, '#', rDbtm, '#', ' ', ' ']
              -- , "  #########  "
              ]
              <> roomRows
              <> ["  #########  "]

makeInitialBurrowState :: [[AmphipodType]] -> BurrowState
makeInitialBurrowState !initRoomAmphs
    | [ r1Amphipods, r2Amphipods, r3Amphipods, r4Amphipods ] <- initRoomAmphs
      -- , [r2Top,r2Bottom]
      -- , [r3Top,r3Bottom]
      -- , [r4Top,r4Bottom]
      -- ] <- initRoomAmphs
    , hallwayState <- Map.fromList $
                      map (\hs -> (hs, HallSpotState hs Nothing)) $
                      [minBound .. maxBound]
    , roomsState <- Map.fromList
                    [ (RoomA, RoomState RoomA $ V.fromList $ Just <$> r1Amphipods)
                    , (RoomB, RoomState RoomB $ V.fromList $ Just <$> r2Amphipods)
                    , (RoomC, RoomState RoomC $ V.fromList $ Just <$> r3Amphipods)
                    , (RoomD, RoomState RoomD $ V.fromList $ Just <$> r4Amphipods)
                    ]
    , totalMoveCost <- 0
    = BurrowState {..}
    | otherwise =
        panic "unexpected: wrong number of Amphipods"

hallwaySpotOccupant :: BurrowState -> HallSpot -> Maybe AmphipodType
hallwaySpotOccupant BurrowState{..} !hallSpot =
    amphipod =<< Map.lookup hallSpot hallwayState

isHallwaySpotOccupied :: BurrowState -> HallSpot -> Bool
isHallwaySpotOccupied !burrowState !hallSpot =
    isJust $ hallwaySpotOccupant burrowState hallSpot

isFinishedState :: BurrowState -> Bool
isFinishedState BurrowState{..} =
    all isRoomFinished roomsState

-- | Info about the number of moves between a room
-- (the top spot) and a hallway position.
data MoveInfo =
    MoveInfo {
        hallSpotID   :: !HallSpot
      , roomID       :: !Room
      , nSteps       :: !StepCount
      , spotsCrossed :: !(Set HallSpot)
    }
    deriving (Eq, Ord)

makeMoveInfo :: HallSpot -> Room -> StepCount -> [HallSpot] -> MoveInfo
makeMoveInfo !hallSpotID !roomID !nSteps !spotsCrossedList =
    MoveInfo{spotsCrossed = Set.fromList spotsCrossedList, ..}

moveInfoMap :: Map (HallSpot, Room) MoveInfo
moveInfoMap =
    Map.fromList $
    map (\mi -> ((hallSpotID mi, roomID mi), mi)) allMoveInfos

allMoveInfos :: [MoveInfo]
allMoveInfos =
    [ makeMoveInfo HallSpot1 RoomA 3 [HallSpot2]
    , makeMoveInfo HallSpot1 RoomB 5 [HallSpot2,HallSpot3]
    , makeMoveInfo HallSpot1 RoomC 7 [HallSpot2,HallSpot3,HallSpot4]
    , makeMoveInfo HallSpot1 RoomD 9 [HallSpot2,HallSpot3,HallSpot4,HallSpot5]

    , makeMoveInfo HallSpot2 RoomA 2 []
    , makeMoveInfo HallSpot2 RoomB 4 [HallSpot3]
    , makeMoveInfo HallSpot2 RoomC 6 [HallSpot3,HallSpot4]
    , makeMoveInfo HallSpot2 RoomD 8 [HallSpot3,HallSpot4,HallSpot5]

    , makeMoveInfo HallSpot3 RoomA 2 []
    , makeMoveInfo HallSpot3 RoomB 2 []
    , makeMoveInfo HallSpot3 RoomC 4 [HallSpot4]
    , makeMoveInfo HallSpot3 RoomD 6 [HallSpot4,HallSpot5]

    , makeMoveInfo HallSpot4 RoomA 4 [HallSpot3]
    , makeMoveInfo HallSpot4 RoomB 2 []
    , makeMoveInfo HallSpot4 RoomC 2 []
    , makeMoveInfo HallSpot4 RoomD 4 [HallSpot5]

    , makeMoveInfo HallSpot5 RoomA 6 [HallSpot3,HallSpot4]
    , makeMoveInfo HallSpot5 RoomB 4 [HallSpot4]
    , makeMoveInfo HallSpot5 RoomC 2 []
    , makeMoveInfo HallSpot5 RoomD 2 []

    , makeMoveInfo HallSpot6 RoomA 8 [HallSpot3,HallSpot4,HallSpot5]
    , makeMoveInfo HallSpot6 RoomB 6 [HallSpot4,HallSpot5]
    , makeMoveInfo HallSpot6 RoomC 4 [HallSpot5]
    , makeMoveInfo HallSpot6 RoomD 2 []

    , makeMoveInfo HallSpot7 RoomA 9 [HallSpot3,HallSpot4,HallSpot5,HallSpot6]
    , makeMoveInfo HallSpot7 RoomB 7 [HallSpot4,HallSpot5,HallSpot6]
    , makeMoveInfo HallSpot7 RoomC 5 [HallSpot5,HallSpot6]
    , makeMoveInfo HallSpot7 RoomD 3 [HallSpot6]
    ]

runSteps :: TVar MoveCost -> BurrowState -> IO [BurrowState]
runSteps !minMoveCostVar !startingState = do
    curMinCost :: MoveCost <- readTVarIO minMoveCostVar

    -- Stop following a path if the cost is already higher than the best found case so far.
    if totalMoveCost startingState >= curMinCost
        then pure []
        else if isFinishedState startingState
          then do
            atomically $ writeTVar minMoveCostVar $ totalMoveCost startingState
            traceVal "Found final state with cost: " (totalMoveCost startingState) `seq`
                pure [startingState]
          else
            let
                nextStates :: [BurrowState]
                nextStates = nextPossibleBurrowStates startingState
            in
                concatMapM (runSteps minMoveCostVar) nextStates

nextPossibleBurrowStates :: BurrowState -> [BurrowState]
nextPossibleBurrowStates !startingState =
    let
        -- Check if there are any amphipods which can be moved
        -- into their final room positions.
        tryMovingAmphToFinalRoom :: HallSpotState -> Maybe BurrowState
        tryMovingAmphToFinalRoom HallSpotState{..}
            | Just amph <- amphipod
            , finalRoomType <- amphFinalRoom amph
            , Just (destRoomState :: RoomState) <- Map.lookup finalRoomType $ roomsState startingState
            , canMoveAmphToRoom amph destRoomState
            , Just MoveInfo{..} <- Map.lookup (spotID, finalRoomType) moveInfoMap
            , not $ any (isHallwaySpotOccupied startingState) spotsCrossed
              -- Check if there is already an amphipod (of this type) in the room.
            -- , nAmphsAlreadyInRoom :: Int <- nAmphipodsInRoom destRoomState
            -- , moveToRoomPos :: Int <- V.length (roomAmphipods destRoomState) - nAmphsAlreadyInRoom - 1
            , Just moveToRoomPos <- openRoomPosition destRoomState
            , nTotalSteps <- nSteps + moveToRoomPos
            , moveCost <- amphMoveCost amph * nTotalSteps
              -- Update the hallway state to remove the amphipod from its spot.
            , newHallwayState <- Map.insert spotID
                                            (HallSpotState spotID Nothing)
                                            (hallwayState startingState)
              -- Update the room state to include the amphipod in the lowest available spot.
            , updateRoom <- moveAmphToRoom destRoomState
                -- if amphipodInRoom
                -- then destRoomState{topAmphipod    = Just amph}
                -- else destRoomState{bottomAmphipod = Just amph}
            , newRoomsState <- Map.insert finalRoomType
                                          updateRoom
                                          (roomsState startingState)
            =
                -- traceVal "\n\ntryMovingAmphToFinalRoom :: moveToRoomPos =" moveToRoomPos `seq`
                -- traceVal "tryMovingAmphToFinalRoom :: before =" startingState `seq`
                -- traceVal "tryMovingAmphToFinalRoom :: after =" $
                Just BurrowState{ hallwayState  = newHallwayState
                                , roomsState    = newRoomsState
                                , totalMoveCost = totalMoveCost startingState + moveCost
                                }
            | otherwise
            = Nothing

        tryMovingToHallway :: RoomState -> [BurrowState]
        tryMovingToHallway roomSt@RoomState{..} =
            case amphToMoveOutOfRoom roomSt of
            Nothing -> []
            Just (amphToMove, fromRoomPos) ->
                let
                    -- Try moving from this room to the given hall spot.
                    checkHallSpot :: HallSpotState -> Maybe BurrowState
                    checkHallSpot HallSpotState{..}
                        | Just MoveInfo{..} <- Map.lookup (spotID, room) moveInfoMap
                        , Nothing <- amphipod
                        , not $ any (isHallwaySpotOccupied startingState) spotsCrossed
                        , nTotalSteps <- nSteps + fromRoomPos
                        , moveCost <- amphMoveCost amphToMove * nTotalSteps
                        , newHallwayState <- Map.insert spotID
                                                (HallSpotState spotID $ Just amphToMove)
                                                (hallwayState startingState)
                        , updateRoom <- moveAmphFromRoom roomSt
                        , newRoomsState <- Map.insert room
                                                updateRoom
                                                (roomsState startingState)
                        =
                            -- traceVal "\n\tryMovingToHallway :: fromRoomPos =" fromRoomPos `seq`
                            -- traceVal "tryMovingToHallway :: before =" startingState `seq`
                            -- traceVal "tryMovingToHallway :: after =" $
                            Just BurrowState{ hallwayState  = newHallwayState
                                            , roomsState    = newRoomsState
                                            , totalMoveCost = totalMoveCost startingState + moveCost
                                            }
                        | otherwise
                        = Nothing
                in
                    mapMaybe checkHallSpot $ Map.elems $ hallwayState startingState
    in
        case asum $ tryMovingAmphToFinalRoom <$> Map.elems (hallwayState startingState) of
        Just moveToRoomState -> [moveToRoomState]
        Nothing ->
            concatMap tryMovingToHallway $
            Map.elems $
            roomsState startingState

amphToMoveOutOfRoom :: RoomState -> Maybe (AmphipodType, Int)  -- (amphType, roomPosFromTop)
amphToMoveOutOfRoom RoomState{..}
    | all (\maybeAmph -> maybe True (== roomFinalAmph room) maybeAmph) roomAmphipods
    = Nothing
    | (emptySpots, filledSpots) <- V.span isNothing roomAmphipods
    , not $ V.null filledSpots
    , Just firstRoomAmph <- V.head filledSpots
    = Just (firstRoomAmph, V.length emptySpots)
    | otherwise
    = Nothing


-- | Answer: 49936
day23b :: IO ()
day23b = do
    let initRoomAmphs =
            -- [ [AmphB, AmphD, AmphD, AmphA]
            -- , [AmphC, AmphC, AmphB, AmphD]
            -- , [AmphB, AmphB, AmphA, AmphC]
            -- , [AmphD, AmphA, AmphC, AmphA]
            -- ]  -- test
            [ [AmphA, AmphD, AmphD, AmphB]
            , [AmphC, AmphC, AmphB, AmphA]
            , [AmphB, AmphB, AmphA, AmphD]
            , [AmphD, AmphA, AmphC, AmphC]
            ]  -- actual

    let initialState :: BurrowState
        initialState = makeInitialBurrowState initRoomAmphs

    putText "Starting State:"
    print initialState

    -- let nextStates = nextPossibleBurrowStates initialState
    -- putText "Next States:"
    -- traverse_ print nextStates

    res :: Int <- day23a_compute initialState
    print res

-- day23b_compute :: BurrowState -> IO Int
-- day23b_compute !initialState = do
--     minMoveCostVar <- newTVarIO maxBound
--     allFinalStates :: [BurrowState] <- runSteps minMoveCostVar initialState
--     pure $ minimum $ totalMoveCost <$> allFinalStates

