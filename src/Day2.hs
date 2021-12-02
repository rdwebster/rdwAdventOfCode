module Day2
    ( day2a
    , day2b
    ) where

import MyPrelude
import qualified Data.Text as T


-- | Answer: 1746616
day2a :: IO ()
day2a = do
    commands <- day2_readInput "data/day2a_input.txt"

    putText "Commands:"
    traverse_ print commands
    putText ""

    print $ day2a_compute commands

day2a_compute :: [Command] -> Int
day2a_compute !commands =
    let
        initPos = (0, 0)
        (finalH, finalD) = foldl' updatePos initPos commands
    in
        finalH * finalD


type HorizPos = Int
type Depth = Int
type Aim = Int
type Position = (HorizPos, Depth)
type PositionB = (HorizPos, Depth, Aim)

data Command =
    MoveForward Int
  | MoveUp      Int
  | MoveDown    Int
    deriving (Show)

updatePos :: Position -> Command -> Position
updatePos (curH, curD) (MoveForward n) = (curH + n, curD)
updatePos (curH, curD) (MoveUp n)      = (curH, curD - n)
updatePos (curH, curD) (MoveDown n)    = (curH, curD + n)
 
updatePosB :: PositionB -> Command -> PositionB
updatePosB (curH, curD, curAim) (MoveForward n) = (curH + n, curD + curAim * n, curAim)
updatePosB (curH, curD, curAim) (MoveUp n)      = (curH, curD, curAim - n)
updatePosB (curH, curD, curAim) (MoveDown n)    = (curH, curD, curAim + n)


day2_readInput :: FilePath -> IO [Command]
day2_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    return $ mapMaybe day2a_parseLine fileLines


day2a_parseLine :: Text -> Maybe Command
day2a_parseLine !txt =
    asum [ parseCommand "forward " MoveForward txt
         , parseCommand "up "      MoveUp      txt
         , parseCommand "down "    MoveDown    txt
         ]

parseCommand :: Text -> (Int -> Command) -> Text -> Maybe Command
parseCommand prefix mk txt = do
    nTxt <- T.stripPrefix prefix txt
    n <- readMaybe $ T.unpack nTxt
    pure $ mk n


-- | Answer: 1741971043
day2b :: IO ()
day2b = do
    commands <- day2_readInput "data/day2a_input.txt"
    print $ day2b_compute commands

day2b_compute :: [Command] -> Int
day2b_compute !commands =
    let
        initPosB = (0, 0, 0)
        (finalH, finalD, _finalAim) = foldl' updatePosB initPosB commands
    in
        finalH * finalD

