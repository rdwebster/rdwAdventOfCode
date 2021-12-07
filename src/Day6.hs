module Day6
    ( day6a
    , day6b
    ) where

import MyPrelude
import qualified Data.Text as T
import qualified Prelude

import Utils


-- | Answer: 360761
day6a :: IO ()
day6a = do
    fish :: [Fish] <- day6_readInput "data/day6a_input.txt"

    putText "Fish:"
    traverse_ print fish
    putText ""

    print $ day6a_compute fish

day6a_compute :: [Fish] -> Int
day6a_compute !fish =
    schoolSize $
    schoolAfterNDays 80 $
    makeSchool fish

newtype Fish =
    Fish {
        timer :: Int
    }
    deriving (Eq)

instance Show Fish where
    show Fish{..} = show timer

-- -- | Returns the new state for a fish after a day.
-- updateFish :: Fish -> [Fish]
-- updateFish Fish{..}
--     | timer == 0 =
--         [ Fish{timer = 6}
--         , Fish{timer = 8}
--         ]
--     | otherwise = [Fish{timer = timer - 1}]
--
-- fishAfterNDays :: Int -> [Fish] -> [Fish]
-- fishAfterNDays !nDays !origFish =
--     let
--         run :: Int -> [Fish] -> [Fish]
--         run !dayN !curFish
--             | dayN == nDays = curFish
--             | otherwise =
--                 let
--                     nextFish :: [Fish]
--                     nextFish = concatMap updateFish curFish
--                 in
--                     traceWithArg "# fish after " dayN (length curFish) `seq`
--                     run (dayN + 1) nextFish
--     in
--         run 0 origFish

data School =
    School {
        num0 :: !Int
      , num1 :: !Int
      , num2 :: !Int
      , num3 :: !Int
      , num4 :: !Int
      , num5 :: !Int
      , num6 :: !Int
      , num7 :: !Int
      , num8 :: !Int
    }

makeSchool :: [Fish] -> School
makeSchool !fish =
    School{ num0 = length $ filter (== Fish 0) fish
          , num1 = length $ filter (== Fish 1) fish
          , num2 = length $ filter (== Fish 2) fish
          , num3 = length $ filter (== Fish 3) fish
          , num4 = length $ filter (== Fish 4) fish
          , num5 = length $ filter (== Fish 5) fish
          , num6 = length $ filter (== Fish 6) fish
          , num7 = length $ filter (== Fish 7) fish
          , num8 = length $ filter (== Fish 8) fish
          }
schoolSize :: School -> Int
schoolSize School{..} =
    num0 + num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8

updateSchool :: School -> School
updateSchool School{..} =
    School{ num0 = num1
          , num1 = num2
          , num2 = num3
          , num3 = num4
          , num4 = num5
          , num5 = num6
          , num6 = num7 + num0  -- Reset timer on spawning fish
          , num7 = num8
          , num8 = num0     -- Spawn new
          }

schoolAfterNDays :: Int -> School -> School
schoolAfterNDays !nDays !origSchool =
    let
        run :: Int -> School -> School
        run !dayN !curSchool
            | dayN == nDays = curSchool
            | otherwise =
                let
                    nextSchool :: School
                    nextSchool = updateSchool curSchool
                in
                    traceWithArg "# fish after " dayN (schoolSize curSchool) `seq`
                    run (dayN + 1) nextSchool
    in
        run 0 origSchool



day6_readInput :: FilePath -> IO [Fish]
day6_readInput !fileName = do
    fileTxt <- readFile fileName
    return $ day6a_parseLine fileTxt

day6a_parseLine :: Text -> [Fish]
day6a_parseLine !txt =
    map Fish $
    mapMaybe (readMaybe . T.unpack) $
    T.splitOn "," txt


-- | Answer: 1632779838045
day6b :: IO ()
day6b = do
    fish :: [Fish] <- day6_readInput "data/day6a_input.txt"
    print $ day6b_compute fish

day6b_compute :: [Fish] -> Int
day6b_compute !fish =
    schoolSize $
    schoolAfterNDays 256 $
    makeSchool fish

