module Day21
    ( day21a
    , day21b
    ) where

import MyPrelude
-- import Data.List.NonEmpty ((<|))
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified Data.Map.Strict as Map
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


-- | Answer: ???
day21b :: IO ()
day21b = do
    let p1Start = 4
        p2Start = 8
    print $ day21b_compute p1Start p2Start

day21b_compute :: Pos -> Pos -> Int
day21b_compute _p1Start _p2Start =
    panic "xxx"

