{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Day18
    ( day18a
    , day18b
    ) where

import MyPrelude
import qualified Data.List as List
-- import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as PL
import qualified Prelude

-- import Utils


-- | Answer: 3665
day18a :: IO ()
day18a = do
    sfNums :: [SFNum] <- day18_readInput "data/day18a_input.txt"

    putText "Snailfish Nums:"
    traverse_ print sfNums
    putText ""

    print $ day18a_compute sfNums

day18a_compute :: [SFNum] -> Int
day18a_compute !sfNums =
    sfNumMagnitude $
    addSFNumList sfNums

data SFNum =
    SFNum {
        leftNum  :: !(Either Int SFNum)
      , rightNum :: !(Either Int SFNum)
    }
    deriving (Eq)

instance Show SFNum where
    show SFNum{..} =
        mconcat [
            "["
          , either show show leftNum
          , ","
          , either show show rightNum
          , "]"
        ]

sfNumMagnitude :: SFNum -> Int
sfNumMagnitude SFNum{..} =
    let
        leftVal  = either identity sfNumMagnitude leftNum
        rightVal = either identity sfNumMagnitude rightNum
    in
        3 * leftVal + 2 * rightVal

addSFNums :: SFNum -> SFNum -> SFNum
addSFNums sfNum1 sfNum2 =
    reduceSFNum $
    SFNum{ leftNum  = Right sfNum1
         , rightNum = Right sfNum2
         }

addSFNumList :: [SFNum] -> SFNum
addSFNumList sfNums =
    List.foldl1' addSFNums sfNums


reduceSFNum :: SFNum -> SFNum
reduceSFNum !origNum =
    let
        doOneReduction :: SFNum -> SFNum
        doOneReduction !sfNum =
            let
                (explNum, explodingDone) = doExploding_NEW sfNum
            in
                if explodingDone then explNum
                else doSplitting explNum

        reduceUntilDone :: SFNum -> SFNum
        reduceUntilDone !sfNum =
            let
                afterReduce :: SFNum
                afterReduce =
                    -- traceVal "\n\nbefore reduction: " sfNum `seq`
                    -- traceVal "after reduction:  " $
                    doOneReduction sfNum
            in
                if afterReduce == sfNum then sfNum
                else reduceUntilDone afterReduce
    in
        reduceUntilDone origNum


-- | Find the first pair nested in 4 pairs and explode it.
_doExploding_OLD :: SFNum -> (SFNum, Bool)
_doExploding_OLD !origNum =
    let
        explodeLevel4Num :: SFNum -> Maybe SFNum
        explodeLevel4Num SFNum{leftNum = Left lv, rightNum = Right SFNum{leftNum = Left e1, rightNum = Left _e2}} =
            Just $ SFNum{leftNum = Left $ lv + e1, rightNum = Left 0}
        explodeLevel4Num SFNum{leftNum = Right SFNum{leftNum = Left _e1, rightNum = Left e2}, rightNum = Left rv} =
            Just $ SFNum{leftNum = Left 0, rightNum = Left $ rv + e2}
        explodeLevel4Num _sfNum = Nothing

        explodeEither :: Int -> Either Int SFNum -> (Either Int SFNum, Bool)
        explodeEither _level (Left v) = (Left v, False)
        explodeEither level (Right sf@SFNum{..})
            | level == 3
            , Just explodedNum <- explodeLevel4Num sf
            = (Right explodedNum, True)
            | level == 3
            = (Right sf, False)
            | otherwise =
                let
                    leftRes      :: Either Int SFNum
                    leftExploded :: Bool
                    (leftRes, leftExploded) = explodeEither (level + 1) leftNum

                    rightRes      :: Either Int SFNum
                    rightExploded :: Bool
                    (rightRes, rightExploded) = explodeEither (level + 1) rightNum
                in
                    if leftExploded then
                        (Right sf{leftNum = leftRes}, True)
                    else if rightExploded then
                        (Right sf{rightNum = rightRes}, True)
                    else
                        (Right sf, False)
    in
        case explodeEither 0 $ Right origNum of
        (Right newNum, exploded) -> (newNum, exploded)
        (Left _numV, _exploded) -> panic "unexpected single number"

_testExplode :: (SFNum, Bool)
_testExplode =
    let
        sfNum = fromMaybe (panic "failed to parse SFNum") $ parseSFNum "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
    in
        doExploding_NEW sfNum

_checkExplode :: Text -> Text -> IO ()
_checkExplode = _checkTransform $ fst . doExploding_NEW

_checkTransform :: Show a => (SFNum -> a) -> Text -> Text -> IO ()
_checkTransform transformFn src expectedRes =
    let
        sfNum = fromMaybe (panic "failed to parse SFNum") $ parseSFNum src
        res = transformFn sfNum
    in
        if expectedRes == show res then
            putText "OK"
        else
            putText $
            "Expected:  " <> expectedRes <> "\n" <>
            "Actual:    " <> show res


_checkAdd :: [Text] -> Text -> IO ()
_checkAdd srcList expectedRes =
    let
        sfNums = map (\src -> fromMaybe (panic "failed to parse SFNum") $ parseSFNum src) srcList
        res = addSFNumList sfNums
    in
        if expectedRes == show res then
            putText "OK"
        else
            putText $
            "Expected:  " <> expectedRes <> "\n" <>
            "Actual:    " <> show res

_checkAdd1 = _checkAdd ["[1,1]", "[2,2]", "[3,3]", "[4,4]"] "[[[[1,1],[2,2]],[3,3]],[4,4]]"
_checkAdd2 = _checkAdd ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]"] "[[[[3,0],[5,3]],[4,4]],[5,5]]"
_checkAdd3 = _checkAdd ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]"] "[[[[5,0],[7,4]],[5,5]],[6,6]]"
_checkAdd4 = _checkAdd [
                 "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
               , "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
               , "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
               , "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
               , "[7,[5,[[3,8],[1,4]]]]"
               , "[[2,[2,2]],[8,[8,1]]]"
               , "[2,9]"
               , "[1,[[[9,3],9],[[9,0],[0,7]]]]"
               , "[[[5,[7,4]],7],1]"
               , "[[[[4,2],2],6],[8,7]]"
               ]
               "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

_checkExplode1 = _checkExplode "[[[[[9,8],1],2],3],4]" "[[[[0,9],2],3],4]"
_checkExplode2 = _checkExplode "[7,[6,[5,[4,[3,2]]]]]" "[7,[6,[5,[7,0]]]]"
_checkExplode3 = _checkExplode "[[6,[5,[4,[3,2]]]],1]" "[[6,[5,[7,0]]],3]"
_checkExplode4 = _checkExplode "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
_checkExplode5 = _checkExplode "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"


_checkSplit :: Text -> Text -> IO ()
_checkSplit = _checkTransform doSplitting

_checkReduce :: Text -> Text -> IO ()
_checkReduce = _checkTransform reduceSFNum

_checkMagnitude :: Text -> Text -> IO ()
_checkMagnitude = _checkTransform sfNumMagnitude


_checkMagnitude0 = _checkMagnitude "[9,1]" "29"
_checkMagnitude1 = _checkMagnitude "[1,9]" "21"
_checkMagnitude2 = _checkMagnitude "[[9,1],[1,9]]" "129"

_checkMagnitude3 = _checkMagnitude "[[1,2],[[3,4],5]]" "143"
_checkMagnitude4 = _checkMagnitude "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" "1384"
_checkMagnitude5 = _checkMagnitude "[[[[1,1],[2,2]],[3,3]],[4,4]]" "445"
_checkMagnitude6 = _checkMagnitude "[[[[3,0],[5,3]],[4,4]],[5,5]]" "791"
_checkMagnitude7 = _checkMagnitude "[[[[5,0],[7,4]],[5,5]],[6,6]]" "1137"
_checkMagnitude8 = _checkMagnitude "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" "3488"


-- after addition: [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
-- after explode:  [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
-- after explode:  [[[[0,7],4],[15,[0,13]]],[1,1]]
-- after split:    [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
-- after split:    [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
-- after explode:  [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
_checkReduce1 = _checkReduce "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

_checkReduce1a = _checkExplode "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"
_checkReduce1b = _checkExplode "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]" "[[[[0,7],4],[15,[0,13]]],[1,1]]"
_checkReduce1c = _checkSplit "[[[[0,7],4],[15,[0,13]]],[1,1]]" "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
_checkReduce1d = _checkSplit "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
_checkReduce1e = _checkExplode "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]" "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

data ExplodeState =
    ExplodeState {
        hasExploded  :: !Bool
      , leftExplVal  :: !Int
      , rightExplVal :: !Int
    }

noExplodeState :: ExplodeState
noExplodeState = ExplodeState{ hasExploded = False, leftExplVal = 0, rightExplVal = 0}

doExploding_NEW :: SFNum -> (SFNum, Bool)
doExploding_NEW !origNum =
    let
        explodeEither :: Int -> Either Int SFNum -> (Either Int SFNum, ExplodeState)
        explodeEither _level (Left v) = (Left v, noExplodeState)
        explodeEither level (Right sf@SFNum{..})
            | level == 4
            , Left leftExplVal <- leftNum
            , Left rightExplVal <- rightNum
            = (Left 0, ExplodeState{hasExploded = True, ..})

            | level == 4
            = (Right sf, noExplodeState)

            | otherwise =
                let
                    leftRes       :: Either Int SFNum
                    leftExplState :: ExplodeState
                    (leftRes, leftExplState) = explodeEither (level + 1) leftNum

                    rightRes       :: Either Int SFNum
                    rightExplState :: ExplodeState
                    (rightRes, rightExplState) = explodeEither (level + 1) rightNum
                in
                    if hasExploded leftExplState then
                        let
                            updatedRight     :: Either Int SFNum
                            updatedExplState :: ExplodeState
                            (updatedRight, updatedExplState) =
                                (incrementLeftValue (rightExplVal leftExplState) rightNum, leftExplState{rightExplVal = 0})
                        in
                            (Right SFNum{leftNum = leftRes, rightNum = updatedRight}, updatedExplState)
                    else if hasExploded rightExplState then
                        let
                            updatedLeft      :: Either Int SFNum
                            updatedExplState :: ExplodeState
                            (updatedLeft, updatedExplState) =
                                (incrementRightValue (leftExplVal rightExplState) leftNum, rightExplState{leftExplVal = 0})
                        in
                            (Right SFNum{leftNum = updatedLeft, rightNum = rightRes}, updatedExplState)
                    else
                        (Right sf, noExplodeState)
    in
        case explodeEither 0 $ Right origNum of
        (Right newNum, ExplodeState{..}) -> (newNum, hasExploded)
        (Left _numV, _exploded) -> panic "unexpected single number"


incrementLeftValue :: Int -> Either Int SFNum -> Either Int SFNum
incrementLeftValue addVal (Left v) = Left $ v + addVal
incrementLeftValue addVal (Right sf@SFNum{..}) =
    Right sf{leftNum = incrementLeftValue addVal leftNum}

incrementRightValue :: Int -> Either Int SFNum -> Either Int SFNum
incrementRightValue addVal (Left v) = Left $ v + addVal
incrementRightValue addVal (Right sf@SFNum{..}) =
    Right sf{rightNum = incrementRightValue addVal rightNum}

-- | Find the first num which requires splitting and update it.
doSplitting :: SFNum -> SFNum
doSplitting !origNum =
    let
        splitNum :: Int -> SFNum
        splitNum !v =
            SFNum (Left $ v `div` 2) (Left $ (v+1) `div` 2)

        splitEither :: Either Int SFNum -> (Either Int SFNum, Bool)
        splitEither (Left v) | v > 9 = (Right $ splitNum v, True)
        splitEither (Left v) = (Left v, False)
        splitEither (Right sf@SFNum{..}) =
            let
                leftRes   :: Either Int SFNum
                leftSplit :: Bool
                (leftRes, leftSplit) = splitEither leftNum

                rightRes   :: Either Int SFNum
                rightSplit :: Bool
                (rightRes, rightSplit) = splitEither rightNum
            in
                if leftSplit then
                    (Right sf{leftNum = leftRes}, True)
                else if rightSplit then
                    (Right sf{rightNum = rightRes}, True)
                else
                    (Right sf, False)
    in
        case splitEither $ Right origNum of
        (Right newNum, _split) -> newNum
        (Left _numV, _split) -> panic "unexpected single number"

_testSplit :: SFNum
_testSplit =
    let
        sfNum = fromMaybe (panic "failed to parse SFNum") $ parseSFNum "[[[[0,7],4],[15,[0,13]]],[1,1]]"
    in
        doSplitting sfNum


day18_readInput :: FilePath -> IO [SFNum]
day18_readInput !fileName = do
    fileTxt <- readFile fileName
    return $ mapMaybe parseSFNum $ T.lines fileTxt

parseSFNum :: Text -> Maybe SFNum
parseSFNum !sfNumText =
    P.parseMaybe sfNumParser sfNumText


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

-- Examples:
-- [1,2]
-- [[1,2],3]
-- [9,[8,7]]
-- [[1,9],[8,5]]
-- [[[[1,2],[3,4]],[[5,6],[7,8]]],9]
sfNumParser :: Parser SFNum
sfNumParser = do
    symbol "["
    leftNum  <- (Left <$> integer) <|> (Right <$> sfNumParser)
    symbol ","
    rightNum <- (Left <$> integer) <|> (Right <$> sfNumParser)    
    symbol "]"
    pure SFNum{..}


-- | Answer: 4775
day18b :: IO ()
day18b = do
    sfNums :: [SFNum] <- day18_readInput "data/day18a_input.txt"
    print $ day18b_compute sfNums

day18b_compute :: [SFNum] -> Int
day18b_compute !sfNums =
    let
        numPairs :: [(SFNum, SFNum)]
        numPairs = do
            n1 <- sfNums
            n2 <- sfNums
            guard $ n1 /= n2
            pure (n1, n2)
    in
        maximum $
        map (sfNumMagnitude . uncurry addSFNums) numPairs
