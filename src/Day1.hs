module Day1
    ( day1a
    , day1b
    ) where

import MyPrelude
import qualified Data.Text as T


-- | Answer: 1374
day1a :: IO ()
day1a = do
    inVals <- day1_readInput "data/day1a_input.txt"
    print $ day1a_compute inVals

day1a_compute :: [Int] -> Int
day1a_compute !items =
    -- Compare each item to the one before it.
    -- Count the number of cases where it is larger.
    length $
    filter (== True) $
    zipWith (>) (drop 1 items) items

-- Val  Prev
-- 200  199
-- 208  200
-- 210  208
-- 200  210
-- 207  200
-- 240  207
-- 269  240
-- 260  269
-- 263  260
--      263


day1_readInput :: FilePath -> IO [Int]
day1_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    return $ mapMaybe day1a_parseLine $ T.unpack <$> fileLines


day1a_parseLine :: String -> Maybe Int
day1a_parseLine !str =
    readMaybe str

-- | Answer: 1418
day1b :: IO ()
day1b = do
    inVals <- day1_readInput "data/day1a_input.txt"
    print $ day1b_compute inVals

day1b_compute :: [Int] -> Int
day1b_compute !items =
    let
        -- The last 2 items in the first triple will be the same as the first 2 items in the
        -- next triple, so we really only need to compare the first item in the first triple
        -- (the head of the input list) with the last item in the following triple (the 4th item
        -- in the input).
        run :: Int -> [Int] -> Int
        run !curTotal (a1 : a2 : a3 : a4 : rest) =
            run (if a4 > a1 then curTotal + 1 else curTotal) (a2 : a3 : a4 : rest)
        run !curTotal _ = curTotal
    in
        run 0 items

