module Day4
    ( day4a
    , day4b
    ) where

import MyPrelude
import qualified Data.List as List
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Prelude

import Utils


-- | Answer: 89001
day4a :: IO ()
day4a = do
    game :: BingoGame <- day4_readInput "data/day4a_input.txt"

    print game
    putText ""

    let (lastNum, winningBoard) = playGame game

    putText "Last Number drawn:"
    print lastNum
    putText ""

    putText "Winning Board:"
    print winningBoard
    putText ""

    putText "Winning Board Score:"
    print $ boardScore winningBoard
    putText ""

    putText "Score:"
    print $ boardScore winningBoard * lastNum

data BingoGame =
    BingoGame {
        numsDrawn :: ![Int]
      , boards    :: ![Board]
    }

instance Show BingoGame where
    show BingoGame{..} =
        "BingoGame\nnumsDrawn: "
        <> show numsDrawn
        <> "\nBoards:\n"
        <> mapJoin "\n\n" show boards

playRound :: BingoGame -> (Int, BingoGame)
playRound BingoGame{..} =
    case numsDrawn of
    [] -> panic "ran out of numbers to draw"
    nextNum : rest ->
        ( nextNum
        , BingoGame{ numsDrawn = rest
                   , boards    = updateBoard nextNum <$> boards
                   }
        )

-- | Play until the first board wins.
playGame :: BingoGame -> (Int, Board)
playGame !game =
    let
        go :: BingoGame -> (Int, Board)
        go !curGame =
            let
                (num, newGame) = playRound curGame
            in
                case find isWinningBoard $ boards newGame of
                Nothing -> go newGame
                Just winner -> (num, winner)
    in
        go game


-- | Play until the last board wins.
playGameB :: BingoGame -> (Int, Board)
playGameB !game =
    let
        go :: BingoGame -> (Int, Board)
        go !curGame =
            let
                (num, newGame) = playRound curGame
            in
                case List.partition isWinningBoard $ boards newGame of
                ([lastWinning], []) -> (num, lastWinning)
                (_winning, nonWinning) -> go newGame{boards = nonWinning}
    in
        go game


data Board =
    Board {
        cells :: !(Vector Cell)      -- row1+row2+row3+row4+row5
    }

instance Show Board where
    show Board{..} =
        let
            rowTxt :: [Cell] -> String
            rowTxt rowCells =
                mapJoin " " show rowCells
        in
            mapJoin "\n" rowTxt $
            chunksOf 5 $
            V.toList cells

updateBoard :: Int -> Board -> Board
updateBoard !num Board{..} =
    Board{cells = updateCell num <$> cells}

isWinningBoard :: Board -> Bool
isWinningBoard Board{..} =
    let
        rowIndexes :: Int -> [Int]
        rowIndexes !rowN =
            map ((rowN * 5) +) [0..4]

        colIndexes :: Int -> [Int]
        colIndexes !colN =
            map (colN +) [0, 5, 10, 15, 20]

        checkRow :: Int -> Bool
        checkRow !rowN =
            checkIndexes $ rowIndexes rowN

        checkCol :: Int -> Bool
        checkCol !colN =
            checkIndexes $ colIndexes colN

        checkIndexes :: [Int] -> Bool
        checkIndexes !idxs =
            all checkIndex idxs

        checkIndex :: Int -> Bool
        checkIndex !idx =
            cellPicked $ cells V.! idx
    in
        any checkRow [0..4]
        || any checkCol [0..4]

boardScore :: Board -> Int
boardScore Board{..} =
    -- Sum of any unpicked cell values in the board.
    sum $
    map cellVal $
    filter (not . cellPicked) $
    V.toList cells

data Cell =
    Cell {
        cellVal    :: !Int
      , cellPicked :: !Bool
    }

instance Show Cell where
    show Cell{..} | cellPicked = "[" <> showCellVal cellVal <> "]"
                  | otherwise  = " " <> showCellVal cellVal <> " "

showCellVal :: Int -> String
showCellVal !v | v < 10 = " " <> show v
               | otherwise = show v

makeCell :: Int -> Cell
makeCell !cellVal = Cell{cellVal, cellPicked = False}

updateCell :: Int -> Cell -> Cell
updateCell !num c@Cell{..} | cellVal == num = c{cellPicked = True}
                           | otherwise = c


day4_readInput :: FilePath -> IO BingoGame
day4_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt

        numsDrawn :: [Int]
        numsDrawn = maybe [] parseNumsDraws $ head fileLines

        boards :: [Board]
        boards =
            map parseBoard $
            chunksOf 6 $
            drop 2 fileLines

    pure BingoGame{..}


-- 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
--
-- 22 13 17 11  0
--  8  2 23  4 24
-- 21  9 14 16  7
--  6 10  3 18  5
--  1 12 20 15 19
--
--  3 15  0  2 22
--  9 18 13 17  5
-- 19  8  7 25 23
-- 20 11 10 24  4
-- 14 21 16 12  6

parseNumsDraws :: Text -> [Int]
parseNumsDraws !txt =
    mapMaybe (readMaybe . T.unpack) $
    T.splitOn "," txt

parseBoard :: [Text] -> Board
parseBoard !boardLines =
    Board $
    V.fromList $
    concatMap parseBoardRow $
    take 5 boardLines

parseBoardRow :: Text -> [Cell]
parseBoardRow !rowTxt =
    map makeCell $
    mapMaybe (readMaybe . T.unpack) $
    T.splitOn " " $
    T.strip rowTxt


-- | Answer: 7296
day4b :: IO ()
day4b = do
    game :: BingoGame <- day4_readInput "data/day4a_input.txt"

    let (lastNum, winningBoard) = playGameB game

    putText "Last Number drawn:"
    print lastNum
    putText ""

    putText "Winning Board:"
    print winningBoard
    putText ""

    putText "Winning Board Score:"
    print $ boardScore winningBoard
    putText ""

    putText "Score:"
    print $ boardScore winningBoard * lastNum
