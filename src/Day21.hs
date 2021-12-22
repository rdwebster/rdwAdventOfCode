module Day21
    ( day21a
    , day21b
    ) where

import MyPrelude
-- import Data.List.NonEmpty ((<|))
-- import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
-- import qualified Data.Text as T
-- import Data.Vector.Unboxed (Vector)
-- import qualified Data.Vector.Unboxed as V
-- import qualified Prelude

import Utils


-- | Answer: 918081
day21a :: IO ()
day21a = do
    -- let p1Start = 4         -- Test
    --     p2Start = 8
    let p1Start = 10        -- Actual
        p2Start = 9
    print $ day21a_compute p1Start p2Start

day21a_compute :: Pos -> Pos -> Int
day21a_compute !p1Start !p2Start =
    let
        rolls :: [DiceRoll]
        rolls = cycle [1..100]

        -- gameSt = initGameState p1Start p2Start

        endGameState :: GameState
        endGameState = runGame rolls $ initGameState p1Start p2Start

        losingScore :: Score
        losingScore = min (p1Score endGameState) (p2Score endGameState)
    in
        traceVal "endGameState = \n" endGameState `seq`
        losingScore * rollsMade endGameState

type Player    = Int
type Pos       = Int
type Score     = Int
type Count     = Int
type NumWins   = Int
type DiceRoll  = Int
type DiceRolls = [DiceRoll]

data GameState =
    GameState {
        nextPlayer :: !Player 
      , rollsMade  :: !Int
      , p1Position :: !Pos
      , p2Position :: !Pos
      , p1Score    :: !Score
      , p2Score    :: !Score
    }
    deriving (Show)

initGameState :: Pos -> Pos -> GameState
initGameState !p1Start !p2Start =
    GameState{
        nextPlayer = 1
      , rollsMade  = 0
      , p1Position = p1Start
      , p2Position = p2Start
      , p1Score    = 0
      , p2Score    = 0
    }

isWinningGameState :: GameState -> Bool
isWinningGameState GameState{..} =
    p1Score >= 1000
    || p2Score >= 1000

runGame :: DiceRolls -> GameState -> GameState
runGame !rolls0 !gameState0 =
    let
        go :: DiceRolls -> GameState -> GameState
        go !curRolls !curState
            | isWinningGameState curState = curState
            | otherwise =
                let
                    nextRolls :: DiceRolls
                    nextState :: GameState
                    (nextRolls, nextState) = playTurn curRolls curState
                in
                    go nextRolls nextState
    in
        go rolls0 gameState0

playTurn :: DiceRolls -> GameState -> (DiceRolls, GameState)
playTurn (r1 : r2 : r3 : rest_rolls) GameState{..}
    | nextPlayer == 1 =
        let
            newPosition = ((p1Position + r1 + r2 + r3 - 1) `mod` 10) + 1
        in
            (rest_rolls,) $
            GameState{
                nextPlayer = 2
              , rollsMade  = rollsMade + 3
              , p1Position = newPosition
              , p2Position = p2Position
              , p1Score    = p1Score + newPosition
              , p2Score    = p2Score
            }
    | otherwise =
        let
            newPosition = ((p2Position + r1 + r2 + r3 - 1) `mod` 10) + 1
        in
            (rest_rolls,) $
            GameState{
                nextPlayer = 1
              , rollsMade  = rollsMade + 3
              , p1Position = p1Position
              , p2Position = newPosition
              , p1Score    = p1Score
              , p2Score    = p2Score + newPosition
            }
playTurn _rolls _gameState =
    panic "unexpected - no more rolls"


data GameStateB =
    GameStateB {
        p1PositionB :: !Pos
      , p2PositionB :: !Pos
      , p1ScoreB    :: !Score
      , p2ScoreB    :: !Score
    }
    deriving (Eq, Ord, Show)

-- | Instead of tracking each universe separately, keep track of how many universes
-- are in each distinct state (p1/p2 positions and p1/p2 scores).
playTurnB :: Player -> Map GameStateB Count -> Map GameStateB Count
playTurnB !nextPlayer !gameStates =
    let
        newRollCombinations :: [DiceRolls]
        newRollCombinations = do
            r1 <- [1..3]
            r2 <- [1..3]
            r3 <- [1..3]
            pure [r1, r2, r3]

        rollTotals :: [Int]
        rollTotals = sum <$> newRollCombinations

        -- Update each known game state for each of the 27 universes for this turn.
        updateGameState :: GameStateB -> Count -> [(GameStateB, Count)]
        updateGameState !gameState !stateCount =
            map (\rt -> (updateGameStateWithRoll gameState rt, stateCount)) rollTotals

        updateGameStateWithRoll :: GameStateB -> Int -> GameStateB
        updateGameStateWithRoll gs@GameStateB{..} !rollTotal
            | nextPlayer == 1 =
                let
                    newPosition = ((p1PositionB + rollTotal - 1) `mod` 10) + 1
                in
                    gs{ p1PositionB = newPosition
                      , p1ScoreB    = p1ScoreB + newPosition
                      }
            | otherwise =
                let
                    newPosition = ((p2PositionB + rollTotal - 1) `mod` 10) + 1
                in
                    gs{ p2PositionB = newPosition
                      , p2ScoreB    = p2ScoreB + newPosition
                      }
    in
        Map.fromListWith (+) $
        concatMap (\(gs, cnt) -> updateGameState gs cnt) $
        Map.toList gameStates

isWinningGameStateB :: GameStateB -> Bool
isWinningGameStateB GameStateB{..} =
    p1ScoreB >= 21
    || p2ScoreB >= 21

runGameB :: Pos -> Pos -> (NumWins, NumWins)
runGameB !p1Start !p2Start =
    let
        gameStates0 :: Map GameStateB Count
        gameStates0 = Map.singleton (GameStateB p1Start p2Start 0 0) 1

        go :: Player -> NumWins -> NumWins -> Map GameStateB Count -> (NumWins, NumWins)
        go !nextPlayer !numWins1 !numWins2 !activeGameStates
            | Map.null activeGameStates = (numWins1, numWins2)
            | otherwise =
                let
                    updatesGameStates :: Map GameStateB Count
                    updatesGameStates = playTurnB nextPlayer activeGameStates

                    -- Separate out any winning game states.
                    winningGameStates   :: Map GameStateB Count
                    newActiveGameStates :: Map GameStateB Count
                    (winningGameStates, newActiveGameStates) =
                        Map.partitionWithKey (\gs _ -> isWinningGameStateB gs) updatesGameStates

                    p1WinStates :: Map GameStateB Count
                    p2WinStates :: Map GameStateB Count
                    (p1WinStates, p2WinStates) =
                        Map.partitionWithKey (\GameStateB{..} _ -> p1ScoreB > p2ScoreB) winningGameStates

                    newWins1 :: NumWins
                    newWins1 = sum $ Map.elems p1WinStates

                    newWins2 :: NumWins
                    newWins2 = sum $ Map.elems p2WinStates
                in
                    go (if nextPlayer == 1 then 2 else 1)
                       (numWins1 + newWins1)
                       (numWins2 + newWins2)
                       newActiveGameStates
    in
        go 1 0 0 gameStates0

-- | Answer: 158631174219251
day21b :: IO ()
day21b = do
    -- let p1Start = 4         -- Test
    --     p2Start = 8
    let p1Start = 10        -- Actual
        p2Start = 9
    print $ day21b_compute p1Start p2Start

day21b_compute :: Pos -> Pos -> Int
day21b_compute !p1Start !p2Start =
    let
        (p1Wins, p2Wins) = runGameB p1Start p2Start
    in
        traceVal "p1Wins = " p1Wins `seq`
        traceVal "p2Wins = " p2Wins `seq`
        max p1Wins p2Wins
