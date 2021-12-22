{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Day22
    ( day22a
    , day22b
    ) where

import MyPrelude
-- import qualified Data.List as List
-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as PL
import qualified Prelude

-- import Utils

-- | Answer: ???            586502 is too high
day22a :: IO ()
day22a = do
    rebootSteps :: [RebootStep] <-
        -- day22_readInput "data/day22test1_input.txt"        -- small test
        -- day22_readInput "data/day22test2_input.txt"     -- med test
        day22_readInput "data/day22a_input.txt"         -- actual

    putText "Reboot Steps:"
    traverse_ print rebootSteps
    putText ""

    print $ day22a_compute $ mapMaybe (limitRebootStepSize (-50) 50) rebootSteps

day22a_compute :: [RebootStep] -> Int
day22a_compute !rebootSteps =
    Set.size $
    runReboot rebootSteps

type Pos = Int
type Coords = (Pos, Pos, Pos)

data RebootStep =
    RebootStep {
        turnOn :: !Bool
      , xFrom  :: !Pos
      , xTo    :: !Pos
      , yFrom  :: !Pos
      , yTo    :: !Pos
      , zFrom  :: !Pos
      , zTo    :: !Pos
    }
    deriving (Eq)

instance Show RebootStep where
    show RebootStep{..} =
        mconcat [
            if turnOn then "on " else "off "
          , "x="
          , show xFrom
          , ".."
          , show xTo
          , ",y="
          , show yFrom
          , ".."
          , show yTo
          , ",z="
          , show zFrom
          , ".."
          , show zTo
        ]

makeRebootStep :: Bool -> Pos -> Pos -> Pos -> Pos -> Pos -> Pos -> RebootStep
makeRebootStep !turnOn !x1 !x2 !y1 !y2 !z1 !z2 =
    RebootStep {
        turnOn
      , xFrom = min x1 x2
      , xTo   = max x1 x2
      , yFrom = min y1 y2
      , yTo   = max y1 y2
      , zFrom = min z1 z2
      , zTo   = max z1 z2
    }

limitRebootStepSize :: Pos -> Pos -> RebootStep -> Maybe RebootStep
limitRebootStepSize !minPos !maxPos RebootStep{..} =
    let
        limitPos :: Pos -> Pos
        limitPos !p
            | p < minPos = minPos
            | p > maxPos = maxPos
            | otherwise  = p
    in do
        guard $ max xFrom xTo >= minPos
        guard $ min xFrom xTo <= maxPos

        guard $ max yFrom yTo >= minPos
        guard $ min yFrom yTo <= maxPos

        guard $ max zFrom zTo >= minPos
        guard $ min zFrom zTo <= maxPos

        pure RebootStep{
            turnOn
          , xFrom = limitPos xFrom
          , xTo   = limitPos xTo
          , yFrom = limitPos yFrom
          , yTo   = limitPos yTo
          , zFrom = limitPos zFrom
          , zTo   = limitPos zTo
        }

runReboot :: [RebootStep] -> Set Coords
runReboot !rebootSteps =
    foldl' runRebootStep Set.empty rebootSteps

runRebootStep :: Set Coords -> RebootStep -> Set Coords
runRebootStep !onCells RebootStep{..} =
    let
        cellsToChange :: Set Coords
        cellsToChange =
            Set.fromList $ do
                x <- [xFrom .. xTo]
                y <- [yFrom .. yTo]
                z <- [zFrom .. zTo]
                pure (x,y,z)
    in
        if turnOn then
            Set.union onCells cellsToChange
        else
            Set.difference onCells cellsToChange

day22_readInput :: FilePath -> IO [RebootStep]
day22_readInput !fileName = do
    fileTxt <- readFile fileName
    return $ mapMaybe parseRebootStep $ T.lines fileTxt

parseRebootStep :: Text -> Maybe RebootStep
parseRebootStep !sfNumText =
    P.parseMaybe rebootStepParser sfNumText


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

signedInteger :: Parser Int
signedInteger = PL.signed sc integer

-- rword :: Text -> Parser ()
-- rword w = (lexeme . P.try) (PC.string w *> P.notFollowedBy PC.alphaNumChar)

-- comma :: Parser ()
-- comma = symbol ","

-- pwdParser :: Parser Text
-- pwdParser = lexeme $ do
--     T.pack <$> P.many PC.letterChar

-- Examples:
-- on x=11..13,y=11..13,z=11..13
-- off x=9..11,y=9..11,z=9..11
rebootStepParser :: Parser RebootStep
rebootStepParser = do
    turnOn <- (symbol "on" >> pure True) <|> (symbol "off" >> pure False)
    symbol "x"
    symbol "="
    xFrom <- signedInteger
    symbol ".."
    xTo   <- signedInteger
    symbol ","
    symbol "y"
    symbol "="
    yFrom <- signedInteger
    symbol ".."
    yTo   <- signedInteger
    symbol ","
    symbol "z"
    symbol "="
    zFrom <- signedInteger
    symbol ".."
    zTo   <- signedInteger
    pure $ makeRebootStep turnOn xFrom xTo yFrom yTo zFrom zTo


-- | Answer: ???
day22b :: IO ()
day22b = do
    rebootSteps :: [RebootStep] <-
        -- day22_readInput "data/day22test1_input.txt"     -- small test
        -- day22_readInput "data/day22test2_input.txt"     -- med test
        day22_readInput "data/day22test3_input.txt"     -- large test
        -- day22_readInput "data/day22a_input.txt"            -- actual
    print $ day22b_compute rebootSteps

day22b_compute :: [RebootStep] -> Int
day22b_compute _rebootSteps =
    panic "xxx"
