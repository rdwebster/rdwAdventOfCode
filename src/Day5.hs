module Day5
    ( day5a
    , day5b
    ) where

import MyPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as PL
-- import qualified Prelude

-- import Utils


-- | Answer: 6548
day5a :: IO ()
day5a = do
    ventLines :: [Line] <- day5_readInput "data/day5a_input.txt"

    putText "Vent Lines:"
    traverse_ print ventLines
    putText ""

    print $ day5a_compute ventLines

day5a_compute :: [Line] -> Int
day5a_compute !ventLines =
    let
        allPoints :: [Coord]
        allPoints = concatMap hvLinePoints ventLines

        pointOccurences :: Map Coord Int
        pointOccurences =
            -- traceMap "pointOccurences = " $
            Map.fromListWith (+) $
            map (\p -> (p, 1)) allPoints
    in
        Map.size $
        -- traceMap "pointOccurences > 1 = " $
        Map.filter (> 1) pointOccurences

type Pos = Int
type Coord = (Pos, Pos)

data Line =
    Line {
        startCoord :: !Coord
      , endCoord   :: !Coord
    }
    deriving (Show)

hvLinePoints :: Line -> [Coord]
hvLinePoints (Line (x1,y1) (x2,y2)) = do
    guard $ x1 == x2 || y1 == y2
    x <- [min x1 x2 .. max x1 x2]
    y <- [min y1 y2 .. max y1 y2]
    pure (x,y)

allLinePoints :: Line -> [Coord]
allLinePoints (Line (x1,y1) (x2,y2))
    | y1 == y2 = do
        -- Horiz line
        x <- [min x1 x2 .. max x1 x2]
        pure (x, y1)
    | x1 == x2 = do
        -- Vert line
        y <- [min y1 y2 .. max y1 y2]
        pure (x1, y)
    | x1 > x2 = allLinePoints (Line (x2,y2) (x1,y1))
    | otherwise =
        -- Diagonal line
        let
            xs = [x1 .. x2]
            ys = if y2 > y1 then [y1 .. y2] else [y1, (y1-1) .. y2]
        in
            zip xs ys

day5_readInput :: FilePath -> IO [Line]
day5_readInput !fileName = do
    fileTxt <- readFile fileName
    return $ mapMaybe parseVentLine $ T.lines fileTxt

-- | Parse a vent line entry from the input text line.
parseVentLine :: Text -> Maybe Line
parseVentLine !ventLineText =
    P.parseMaybe ventLineParser ventLineText


type Parser = P.Parsec Void Text

-- | Space consumer parser.
sc :: Parser ()
sc = PL.space PC.space1 lineCmnt blockCmnt
    where
        lineCmnt  = PL.skipLineComment "//"
        blockCmnt = PL.skipBlockComment "/*" "*/"
    
lexeme :: Parser a -> Parser a
lexeme = PL.lexeme sc

symbol :: Text -> Parser ()
symbol = void . PL.symbol sc

-- parens :: Parser a -> Parser a
-- parens = P.between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme PL.decimal

-- signedInteger :: Parser Int
-- signedInteger = PL.signed sc integer

-- rword :: Text -> Parser ()
-- rword w = (lexeme . P.try) (PC.string w *> P.notFollowedBy PC.alphaNumChar)

-- comma :: Parser ()
-- comma = symbol ","

-- pwdParser :: Parser Text
-- pwdParser = lexeme $ do
--     T.pack <$> P.many PC.letterChar

-- Example:
-- 0,9 -> 5,9
-- 8,0 -> 0,8
-- 9,4 -> 3,4
ventLineParser :: Parser Line
ventLineParser = do
    x1 <- integer
    symbol ","
    y1 <- integer
    symbol "->"
    x2 <- integer
    symbol ","
    y2 <- integer
    pure $ Line (x1,y1) (x2,y2)


-- | Answer: 19663
day5b :: IO ()
day5b = do
    ventLines :: [Line] <- day5_readInput "data/day5a_input.txt"
    print $ day5b_compute ventLines

day5b_compute :: [Line] -> Int
day5b_compute !ventLines =
    let
        allPoints :: [Coord]
        allPoints = concatMap allLinePoints ventLines

        pointOccurences :: Map Coord Int
        pointOccurences =
            -- traceMap "pointOccurences = " $
            Map.fromListWith (+) $
            map (\p -> (p, 1)) allPoints
    in
        Map.size $
        -- traceMap "pointOccurences > 1 = " $
        Map.filter (> 1) pointOccurences

