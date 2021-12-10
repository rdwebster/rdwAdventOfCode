module Day10
    ( day10a
    , day10b
    ) where

import MyPrelude
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
import qualified Data.Text as T
-- import qualified Prelude

import Utils


-- | Answer: 415953
day10a :: IO ()
day10a = do
    chunks :: [Chunk] <- day10_readInput "data/day10a_input.txt"

    putText "Chunks:"
    traverse_ putText chunks
    putText ""

    print $ day10a_compute chunks

day10a_compute :: [Chunk] -> Int
day10a_compute !chunks =
    sum $
    mapMaybe checkChunkSyntax chunks

type Chunk = Text
type SyntaxErrorScore = Int

-- | If the chunk is invalid, then return the score corresponding
-- to the first invalid char.
checkChunkSyntax :: Chunk -> Maybe SyntaxErrorScore
checkChunkSyntax !chunk =
    let
        checkChunk :: [Char] -> [Char] -> Maybe SyntaxErrorScore
        checkChunk _openCharsStack [] = Nothing
        checkChunk openCharsStack (c : c_rest) | isOpenChar c =
            checkChunk (c : openCharsStack) c_rest
        checkChunk (s : s_rest) (c : c_rest) | isCloseChar c =
            if c == matchingCloseChar s then checkChunk s_rest c_rest
            else Just $ illegalClosingCharScore c
        checkChunk openCharsStack c_rest =
            panic $ "checkChunkSyntax: openCharsStack = " <> T.pack openCharsStack <> ", chunk = " <> T.pack c_rest
    in
        checkChunk [] $ T.unpack chunk

matchingCloseChar :: Char -> Char
matchingCloseChar '(' = ')'
matchingCloseChar '<' = '>'
matchingCloseChar '{' = '}'
matchingCloseChar '[' = ']'
matchingCloseChar c   = panic $ "matchingCloseChar :: not a closing char: " <> show c

isOpenChar :: Char -> Bool
isOpenChar !c = c `elem` ['(', '[', '{', '<']

isCloseChar :: Char -> Bool
isCloseChar !c = c `elem` [')', ']', '}', '>']

illegalClosingCharScore :: Char -> SyntaxErrorScore
illegalClosingCharScore ')' = 3
illegalClosingCharScore ']' = 57
illegalClosingCharScore '}' = 1197
illegalClosingCharScore '>' = 25137
illegalClosingCharScore c   = panic $ "illegalClosingCharScore :: not a closing char: " <> show c


day10_readInput :: FilePath -> IO [Chunk]
day10_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    pure fileLines


-- | Answer: ???
day10b :: IO ()
day10b = do
    chunks :: [Chunk] <- day10_readInput "data/day10a_input.txt"
    print $ day10b_compute chunks

day10b_compute :: [Chunk] -> Int
day10b_compute _chunks =
    panic "xxx"
