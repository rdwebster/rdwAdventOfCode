module Day20
    ( day20a
    , day20b
    ) where

import MyPrelude
-- import Data.List.NonEmpty ((<|))
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Prelude

import Utils


-- | Answer: 5483
day20a :: IO ()
day20a = do
    (enhAlgorithm, image) :: (EnhAlgorithm, Image) <-
        -- day20_readInput "data/day20test_input.txt"
        day20_readInput "data/day20a_input.txt"

    putText "Image Enhancement Algorithm:"
    print enhAlgorithm
    putText ""

    putText "Image:"
    print image
    putText ""

    print $ day20a_compute enhAlgorithm image

day20a_compute :: EnhAlgorithm -> Image -> Int
day20a_compute !enhAlgorithm !image =
    let
        enhImg1 :: Image
        enhImg1 = enhanceImage enhAlgorithm False image

        enhImg2 :: Image
        enhImg2 = enhanceImage enhAlgorithm False enhImg1
    in
        traceVal "enhImg1 = \n" enhImg1 `seq`
        traceVal "enhImg2 = \n" enhImg2 `seq`
        Set.size $ lightPixels enhImg2


type EnhAlgorithm = Vector Bool

type Coords = (Int, Int)

data Image =
    Image {
        lightPixels :: !(Set Coords)
      , inverted    :: !Bool
    }

instance Show Image where
    show img@Image{..} =
        let
            minX = minimum $ fst <$> Set.toList lightPixels
            maxX = maximum $ fst <$> Set.toList lightPixels
            minY = minimum $ snd <$> Set.toList lightPixels
            maxY = maximum $ snd <$> Set.toList lightPixels

            rowStr :: Int -> String
            rowStr !y =
                let
                    cellChar :: Int -> Char
                    cellChar !x
                        | isPixelLit img (x,y) = '#'
                        | otherwise = '.'
                in
                    cellChar <$> [minX .. maxX]
        in
            Prelude.unlines $
            rowStr <$> [minY .. maxY]

isPixelLit :: Image -> Coords -> Bool
isPixelLit Image{..} !c
    | inverted = not $ Set.member c lightPixels
    | otherwise = Set.member c lightPixels

enhanceImage :: EnhAlgorithm -> Bool -> Image -> Image
enhanceImage !enhAlgorithm !alternateInverted img@Image{..} =
    let
        minX = minimum $ fst <$> Set.toList lightPixels
        maxX = maximum $ fst <$> Set.toList lightPixels
        minY = minimum $ snd <$> Set.toList lightPixels
        maxY = maximum $ snd <$> Set.toList lightPixels

        shouldLightPixel :: Coords -> Bool
        shouldLightPixel (x, y) =
            let
                pixelsToCheck :: [Coords]
                pixelsToCheck =
                    [ (x-1,y-1), (x,y-1), (x+1,y-1)
                    , (x-1,y),   (x,y),   (x+1,y)
                    , (x-1,y+1), (x,y+1), (x+1,y+1)
                    ]

                pixelsLit :: [Bool]
                pixelsLit =
                    isPixelLit img <$> pixelsToCheck

                enhIndx :: Int
                enhIndx = bitsToInt pixelsLit
            in
                if newInverted then
                    not (enhAlgorithm V.! enhIndx)
                else
                    enhAlgorithm V.! enhIndx

        padding = 1

        newLightPixels :: Set Coords
        newLightPixels =
            Set.fromList $
            filter shouldLightPixel $ do
                x <- [(minX-padding)..(maxX+padding)]
                y <- [(minY-padding)..(maxY+padding)]
                pure (x, y)

        newInverted :: Bool
        newInverted =
            if alternateInverted then not inverted
            else inverted
    in
        Image newLightPixels newInverted

bitsToInt :: [Bool] -> Int
bitsToInt !bitsSet =
    foldl' (\cur b -> cur * 2 + (if b then 1 else 0)) 0 bitsSet


day20_readInput :: FilePath -> IO (EnhAlgorithm, Image)
day20_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
        enhAlgorithm = day20_parseImgEnhAlgorithm $ unsafeHead fileLines
        image = day20_parseImage $ drop 2 fileLines
    return (enhAlgorithm, image)

day20_parseImgEnhAlgorithm :: Text -> EnhAlgorithm
day20_parseImgEnhAlgorithm !enhAlgoText =
    V.fromList $
    map (== '#') $
    T.unpack enhAlgoText

day20_parseImage :: [Text] -> Image
day20_parseImage !imageLines =
    let
        parseRow :: Int -> Text -> Set Coords
        parseRow !y !rowTxt =
            Set.fromList $
            map (\pr -> (fst pr, y)) $
            filter (\pr -> snd pr == '#') $
            zip [0..] $
            T.unpack rowTxt

        lightPixels :: Set Coords
        lightPixels =
            mconcat $
            zipWith parseRow [0..] imageLines

        inverted :: Bool
        inverted = False
    in
        Image{..}


-- | Answer: 18732
day20b :: IO ()
day20b = do
    (enhAlgorithm, image) :: (EnhAlgorithm, Image) <-
        -- day20_readInput "data/day20test_input.txt"
        day20_readInput "data/day20a_input.txt"

    print $ day20b_compute enhAlgorithm image

day20b_compute :: EnhAlgorithm -> Image -> Int
day20b_compute !enhAlgorithm !image =
    let
        n = 50
        
        -- Note that the background will alternate between light and dark for the full puzzle input.
        -- Perhpas try changing how the points are represented to be the light or dark pixels would work better here.
        enhImg :: Image
        enhImg =
            iterate (enhanceImage enhAlgorithm True) image !! n
    in
        traceVal "enhImg = \n" enhImg `seq`
        Set.size $ lightPixels enhImg

