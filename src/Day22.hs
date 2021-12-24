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

import Utils

-- | Answer: 556501
day22a :: IO ()
day22a = do
    rebootSteps :: [RebootStep] <-
        -- day22_readInput "data/day22test1_input.txt"     -- small test
        -- day22_readInput "data/day22test2_input.txt"     -- med test
        day22_readInput "data/day22a_input.txt"            -- actual

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

data Range =
    Range {
        rangeStart :: !Pos
      , rangeEnd   :: !Pos
    }
    deriving (Eq, Ord)

instance Show Range where
    show Range{..} =
        mconcat [
            show rangeStart
          , ".."
          , show rangeEnd
          ]

makeRange :: Pos -> Pos -> Range
makeRange !v1 !v2 =
    Range {
        rangeStart = min v1 v2
      , rangeEnd   = max v1 v2
    }

rangeSize :: Range -> Int
rangeSize Range{..} =
    (rangeEnd - rangeStart + 1)

rangeValues :: Range -> [Pos]
rangeValues Range{..} =
    [rangeStart .. rangeEnd]

isSubRange :: Range -> Range -> Bool
isSubRange !range1 !range2 =
    rangeStart range1 >= rangeStart range2
    && rangeEnd range1 <= rangeEnd range2

rangesOverlap :: Range -> Range -> Bool
rangesOverlap !r1 !r2 =
    rangeStart r1    `inRange` r2
    || rangeEnd r1   `inRange` r2
    || rangeStart r2 `inRange` r1
    || rangeEnd r2   `inRange` r1

inRange :: Pos -> Range -> Bool
inRange !v Range{..} =
    v >= rangeStart && v <= rangeEnd

data Region =
    Region {
        xRange :: !Range
      , yRange :: !Range
      , zRange :: !Range
    }
    deriving (Eq, Ord)

instance Show Region where
    show Region{..} =
        mconcat [
            "x="
          , show xRange
          , ",y="
          , show yRange
          , ",z="
          , show zRange
        ]

makeRegion :: Pos -> Pos -> Pos -> Pos -> Pos -> Pos -> Region
makeRegion !x1 !x2 !y1 !y2 !z1 !z2 =
    Region {
        xRange = makeRange x1 x2
      , yRange = makeRange y1 y2
      , zRange = makeRange z1 z2
    }

-- | Returns the number of cubes in a region.
regionSize :: Region -> Int
regionSize Region{..} =
    rangeSize xRange
    * rangeSize yRange
    * rangeSize zRange

limitRegionSize :: Pos -> Pos -> Region -> Maybe Region
limitRegionSize !minPos !maxPos Region{..} =
    let
        limitPos :: Pos -> Pos
        limitPos !p
            | p < minPos = minPos
            | p > maxPos = maxPos
            | otherwise  = p

        limitRange :: Range -> Maybe Range
        limitRange Range{..} = do
            guard $ rangeEnd >= minPos
            guard $ rangeStart <= maxPos
            pure Range{ rangeStart = limitPos rangeStart
                      , rangeEnd   = limitPos rangeEnd
                      }
    in do
        newXRange <- limitRange xRange
        newYRange <- limitRange yRange
        newZRange <- limitRange zRange
        pure Region{ xRange = newXRange
                   , yRange = newYRange
                   , zRange = newZRange
                   }

-- | Returns True if the first region fits completely within the 2nd region.
isSubRegion :: Region -> Region -> Bool
isSubRegion !region1 !region2 =
    isSubRange (xRange region1) (xRange region2)
    && isSubRange (yRange region1) (yRange region2)
    && isSubRange (zRange region1) (zRange region2)

regionsOverlap :: Region -> Region -> Bool
regionsOverlap !region1 !region2 =
    rangesOverlap (xRange region1) (xRange region2)
    && rangesOverlap (yRange region1) (yRange region2)
    && rangesOverlap (zRange region1) (zRange region2)

splitRange :: Pos -> Range -> [Range]
splitRange !splitPos r@Range{..}
    | splitPos < rangeStart || splitPos >= rangeEnd
    = [r]
    | otherwise
    = [ r{rangeEnd = splitPos}, r{rangeStart = splitPos + 1} ]

splitRegionX :: Pos -> Region -> [Region]
splitRegionX !xSplit r@Region{..}
    | xRanges <- splitRange xSplit xRange
    = map (\xr -> r{xRange = xr}) xRanges

splitRegionY :: Pos -> Region -> [Region]
splitRegionY !ySplit r@Region{..}
    | yRanges <- splitRange ySplit yRange
    = map (\yr -> r{yRange = yr}) yRanges

splitRegionZ :: Pos -> Region -> [Region]
splitRegionZ !zSplit r@Region{..}
    | zRanges <- splitRange zSplit zRange
    = map (\zr -> r{zRange = zr}) zRanges


data RebootStep =
    RebootStep {
        turnOn :: !Bool
      , region :: !Region
    }
    deriving (Eq)

instance Show RebootStep where
    show RebootStep{..} =
        mconcat [
            if turnOn then "on " else "off "
          , show region
        ]

limitRebootStepSize :: Pos -> Pos -> RebootStep -> Maybe RebootStep
limitRebootStepSize !minPos !maxPos RebootStep{..} = do
    region' <- limitRegionSize minPos maxPos region
    pure RebootStep{ turnOn, region = region' }

runReboot :: [RebootStep] -> Set Coords
runReboot !rebootSteps =
    foldl' runRebootStep Set.empty rebootSteps

runRebootStep :: Set Coords -> RebootStep -> Set Coords
runRebootStep !onCells RebootStep{turnOn, region = Region{..}} =
    let
        cellsToChange :: Set Coords
        cellsToChange =
            Set.fromList $ do
                x <- rangeValues xRange
                y <- rangeValues yRange
                z <- rangeValues zRange
                pure (x,y,z)
    in
        if turnOn then
            Set.union onCells cellsToChange
        else
            Set.difference onCells cellsToChange

runRebootB :: [RebootStep] -> Set Region
runRebootB !rebootSteps =
    foldl' runRebootStepB Set.empty rebootSteps

runRebootStepB :: Set Region -> RebootStep -> Set Region
runRebootStepB !onRegions rs@RebootStep{..} =
    let
        -- Remove the new region from any existing one.
        updatedOnRegions :: Set Region
        updatedOnRegions =
            Set.fromList $
            concatMap (updateRegion region) onRegions
    in
        traceVal "Running reboot step: " rs `seq`
        (if turnOn then Set.insert region else identity) updatedOnRegions

-- | Remove, modify, or split a region such that it doesn't overlap
-- with the new region being set or unset.
updateRegion :: Region -> Region -> [Region]
updateRegion !newRegion !oldRegion
      -- If the new region doesn't overlap this old one
      -- then leave the old region alone.
    | not $ regionsOverlap newRegion oldRegion
    = [oldRegion]
      -- If the old region is completely contained within the new
      -- region, then we can simply drop the old one.
    | isSubRegion oldRegion newRegion
    = []

    | (xSplit : _) <- splitPoints (xRange newRegion) (xRange oldRegion)
    , splitRs <- splitRegionX xSplit oldRegion
    =
        -- traceVal "____ X-splitRs = " splitRs `seq`
        -- traceVal "____ X-split :: RES = " $
        concatMap (updateRegion newRegion) splitRs

    | (ySplit : _) <- splitPoints (yRange newRegion) (yRange oldRegion)
    , splitRs <- splitRegionY ySplit oldRegion
    =
        -- traceVal "____ Y-splitRs = " splitRs `seq`
        -- traceVal "____ Y-split :: RES = " $
        concatMap (updateRegion newRegion) splitRs

    | (zSplit : _) <- splitPoints (zRange newRegion) (zRange oldRegion)
    , splitRs <- splitRegionZ zSplit oldRegion
    =
        -- traceVal "____ Z-splitRs = " splitRs `seq`
        -- traceVal "____ Z-split :: RES = " $
        concatMap (updateRegion newRegion) splitRs

    | otherwise
    = traceVal "oldRegion = " oldRegion `seq`
      traceVal "newRegion = " newRegion `seq`
      panic "updateRegion: unexpected case"       --TODO: can this case happen?

-- | Compute the points at which to split the old region
-- based on the new region start/end postions.
splitPoints :: Range -> Range -> [Pos]
splitPoints rNew@Range{rangeStart = newStart, rangeEnd = newEnd}
            rOld@Range{rangeStart = oldStart, rangeEnd = oldEnd}
    | not $ rangesOverlap rNew rOld
    = []
    | isSubRange rOld rNew
    = []
    | oldStart < newStart && oldEnd > newEnd
    = [newStart - 1, newEnd]
    | oldStart < newStart
    = [newStart - 1]
    | otherwise
    = [newEnd]


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
    let region = makeRegion xFrom xTo yFrom yTo zFrom zTo
    pure RebootStep{..} 


-- | Answer: 1217140271559773
day22b :: IO ()
day22b = do
    rebootSteps :: [RebootStep] <-
        -- day22_readInput "data/day22test3_input.txt"     -- large test
        day22_readInput "data/day22a_input.txt"            -- actual
    print $ day22b_compute rebootSteps

day22b_compute :: [RebootStep] -> Int
day22b_compute !rebootSteps =
    sum $
    map regionSize $
    Set.toList $
    runRebootB rebootSteps
