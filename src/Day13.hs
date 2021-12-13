module Day13
    ( day13a
    , day13b
    ) where

import MyPrelude
-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

import Utils


-- | Answer: 706
day13a :: IO ()
day13a = do
    (paper, instrs) :: (Paper, [FoldInstruction]) <- day13_readInput "data/day13a_input.txt"
    print $ day13a_compute paper instrs

day13a_compute :: Paper -> [FoldInstruction] -> Int
day13a_compute !paper !instrs =
    nVisibleDotsOnPaper $
    foldPaper paper $
    unsafeHead instrs

type Coords = (Int,Int)  -- x,y

data Paper =
    Paper {
        dots :: Set Coords
    }

instance Show Paper where
    show Paper{..} =
        let
            minX = minimum $ fst <$> Set.toList dots
            maxX = maximum $ fst <$> Set.toList dots
            minY = minimum $ snd <$> Set.toList dots
            maxY = maximum $ snd <$> Set.toList dots

            rowStr :: Int -> String
            rowStr !y =
                let
                    cellChar :: Int -> Char
                    cellChar !x
                        | Set.member (x,y) dots = '#'
                        | otherwise = '.'
                in
                    cellChar <$> [minX .. maxX]
        in
            Prelude.unlines $
            rowStr <$> [minY .. maxY]

nVisibleDotsOnPaper :: Paper -> Int
nVisibleDotsOnPaper = Set.size . dots

data FoldInstruction =
    FoldInstruction {
        foldDirection :: !FoldDirection
      , foldPosition  :: !Int
    }

data FoldDirection =
    FoldUp
  | FoldLeft


foldPaper :: Paper -> FoldInstruction -> Paper
foldPaper Paper{..} foldInstrs =
    Paper $ Set.map (transformDot foldInstrs) dots


transformDot :: FoldInstruction -> Coords -> Coords
transformDot FoldInstruction{foldDirection = FoldUp, ..} (x,y) =
    let
        newY :: Int
        newY =
            if y <= foldPosition then y
            else 2*foldPosition - y
    in
        (x, newY)
transformDot FoldInstruction{foldDirection = FoldLeft, ..} (x,y) =
    let
        newX :: Int
        newX =
            if x <= foldPosition then x
            else 2*foldPosition - x
    in
        (newX, y)


day13_readInput :: FilePath -> IO (Paper, [FoldInstruction])
day13_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    let paper = Paper $ Set.fromList $ mapMaybe day13a_parseLineCoords fileLines
    let instrs = mapMaybe day13a_parseLineFoldInstruction fileLines
    pure (paper, instrs)

day13a_parseLineCoords :: Text -> Maybe Coords
day13a_parseLineCoords !txt
    | [xTxt, yTxt] <- T.splitOn "," txt
    , Just x <- textToInt xTxt
    , Just y <- textToInt yTxt
    = Just (x,y)
    | otherwise
    = Nothing

day13a_parseLineFoldInstruction :: Text -> Maybe FoldInstruction
day13a_parseLineFoldInstruction !txt =
    asum [
        do  posTxt <- T.stripPrefix "fold along y=" txt
            pos    <- textToInt posTxt
            pure $ FoldInstruction FoldUp pos
      , do  posTxt <- T.stripPrefix "fold along x=" txt
            pos    <- textToInt posTxt
            pure $ FoldInstruction FoldLeft pos
    ]


-- | Answer: LRFJBJEH
day13b :: IO ()
day13b = do
    (paper, instrs) :: (Paper, [FoldInstruction]) <- day13_readInput "data/day13a_input.txt"
    print $ day13b_compute paper instrs

day13b_compute :: Paper -> [FoldInstruction] -> Paper
day13b_compute !paper !instrs =
    foldl' foldPaper paper instrs
