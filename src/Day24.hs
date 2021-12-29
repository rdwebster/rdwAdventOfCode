module Day24
    ( day24a
    , day24b
    ) where

import MyPrelude
-- import qualified Data.List as List
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as PL
import qualified Prelude

-- import Utils

-- | Answer: 96299896449997
day24a :: IO ()
day24a = do
    program :: Program <- day24_readInput "data/day24a_input.txt"

    putText "Program Instructions:"
    traverse_ print program
    putText ""

    let largestValidInput = day24a_compute program
    putText "Largest valid input:"
    print largestValidInput
    print $ foldl' (\cur dig -> cur * 10 + dig) 0 largestValidInput

    print $ runProgram largestValidInput program


day24a_compute :: Program -> [Value]
day24a_compute !program =
    let
        -- Rules:
        -- D6  = D5-1
        -- D8  = D7-3
        -- D9  = D4-5
        -- D11 = D10+5
        -- D12 = D3+7
        -- D13 = D2+3
        -- D14 = D1-2
        digitCombos :: [[Value]]
        digitCombos = do
            d1 <- [9,8..1]
            d2 <- [9,8..1]
            d3 <- [9,8..1]
            d4 <- [9,8..1]
            d5 <- [9,8..1]
            let d6 = d5 - 1
            guard $ d6 > 0
            d7 <- [9,8..1]
            let d8 = d7 - 3
            guard $ d8 > 0
            let d9 = d4 - 5
            guard $ d9 > 0
            d10 <- [9,8..1]
            let d11 = d10 + 5
            guard $ d11 < 10
            let d12 = d3 + 7
            guard $ d12 < 10
            let d13 = d2 + 3
            guard $ d13 < 10
            let d14 = d1 - 2
            guard $ d14 > 0
            pure [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14]
    in
        -- foldl' (\cur dig -> cur * 10 + dig) 0 $
        unsafeHead digitCombos

type Program = [Instruction]
type Value = Int

data Variable =
    Var_w
  | Var_x
  | Var_y
  | Var_z

instance Show Variable where
    show Var_w = "w"
    show Var_x = "x"
    show Var_y = "y"
    show Var_z = "z"

data Instruction =
    Inp_Instruction {
        var :: !Variable
    } |
    Add_Instruction {
        var :: !Variable
      , arg :: !(Either Variable Value)
    } |
    Mul_Instruction {
        var :: !Variable
      , arg :: !(Either Variable Value)
    } |
    Div_Instruction {
        var :: !Variable
      , arg :: !(Either Variable Value)
    } |
    Mod_Instruction {
        var :: !Variable
      , arg :: !(Either Variable Value)
    } |
    Eql_Instruction {
        var :: !Variable
      , arg :: !(Either Variable Value)
    }

instance Show Instruction where
    show Inp_Instruction{..} =
        "inp " <> show var
    show Add_Instruction{..} =
        "add " <> show var <> " " <> either show show arg
    show Mul_Instruction{..} =
        "mul " <> show var <> " " <> either show show arg
    show Div_Instruction{..} =
        "div " <> show var <> " " <> either show show arg
    show Mod_Instruction{..} =
        "mod " <> show var <> " " <> either show show arg
    show Eql_Instruction{..} =
        "eql " <> show var <> " " <> either show show arg

data ProgState =
    ProgState {
        inputs :: ![Value]
      , w      :: !Value
      , x      :: !Value
      , y      :: !Value
      , z      :: !Value
    }
    deriving Show

initialProgState :: [Value] -> ProgState
initialProgState !inputs =
    ProgState inputs 0 0 0 0

runProgram :: [Value] -> Program -> ProgState
runProgram !inputs !program =
    foldl' processInstruction (initialProgState inputs) program

processInstruction :: ProgState -> Instruction -> ProgState
processInstruction st@ProgState{..} Inp_Instruction{..} =
    setVariable var (unsafeHead inputs) $
    st{ inputs = drop 1 inputs }
processInstruction st Add_Instruction{..} =
    let
        v1 = getVariable var st
        v2 = either (`getVariable` st) identity arg
    in
        setVariable var (v1 + v2) st
processInstruction st Mul_Instruction{..} =
    let
        v1 = getVariable var st
        v2 = either (`getVariable` st) identity arg
    in
        setVariable var (v1 * v2) st
processInstruction st Div_Instruction{..} =
    let
        v1 = getVariable var st
        v2 = either (`getVariable` st) identity arg
    in
        setVariable var (v1 `div` v2) st
processInstruction st Mod_Instruction{..} =
    let
        v1 = getVariable var st
        v2 = either (`getVariable` st) identity arg
    in
        setVariable var (v1 `mod` v2) st
processInstruction st Eql_Instruction{..} =
    let
        v1 = getVariable var st
        v2 = either (`getVariable` st) identity arg
    in
        setVariable var (if v1 == v2 then 1 else 0) st

getVariable :: Variable -> ProgState -> Value
getVariable Var_w = w
getVariable Var_x = x
getVariable Var_y = y
getVariable Var_z = z

setVariable :: Variable -> Value -> ProgState -> ProgState
setVariable Var_w !v !st = st{ w = v }
setVariable Var_x !v !st = st{ x = v }
setVariable Var_y !v !st = st{ y = v }
setVariable Var_z !v !st = st{ z = v }

day24_readInput :: FilePath -> IO Program
day24_readInput !fileName = do
    fileTxt <- readFile fileName
    return $ mapMaybe parseInstruction $ T.lines fileTxt

parseInstruction :: Text -> Maybe Instruction
parseInstruction !instrText =
    P.parseMaybe instructionParser instrText


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
-- inp w
-- mul x 0
-- add x z
-- mod x 26
instructionParser :: Parser Instruction
instructionParser =
    let
        inpParser :: Parser Instruction
        inpParser = do
            symbol "inp"
            var <- parseVariable
            pure Inp_Instruction{..}

        addParser :: Parser Instruction
        addParser = do
            symbol "add"
            var <- parseVariable
            arg <- parseArg
            pure Add_Instruction{..}

        mulParser :: Parser Instruction
        mulParser = do
            symbol "mul"
            var <- parseVariable
            arg <- parseArg
            pure Mul_Instruction{..}

        divParser :: Parser Instruction
        divParser = do
            symbol "div"
            var <- parseVariable
            arg <- parseArg
            pure Div_Instruction{..}

        modParser :: Parser Instruction
        modParser = do
            symbol "mod"
            var <- parseVariable
            arg <- parseArg
            pure Mod_Instruction{..}

        eqlParser :: Parser Instruction
        eqlParser = do
            symbol "eql"
            var <- parseVariable
            arg <- parseArg
            pure Eql_Instruction{..}
    in
        inpParser <|>
        addParser <|>
        mulParser <|>
        divParser <|>
        modParser <|>
        eqlParser

parseArg :: Parser (Either Variable Value)
parseArg =
    (Left <$> parseVariable)
    <|>
    (Right <$> signedInteger)

parseVariable :: Parser Variable
parseVariable =
    (symbol "w" >> pure Var_w)
    <|>
    (symbol "x" >> pure Var_x)
    <|>
    (symbol "y" >> pure Var_y)
    <|>
    (symbol "z" >> pure Var_z)

-- | Answer: ???
day24b :: IO ()
day24b = do
    program :: Program <- day24_readInput "data/day24a_input.txt"

    putText "Program Instructions:"
    traverse_ print program
    putText ""

    print $ day24b_compute program

day24b_compute :: Program -> Int
day24b_compute _program =
    panic "xxx"