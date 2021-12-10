module Day10
    ( day10a
    , day10b
    ) where

import MyPrelude
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

-- import Utils


-- | Answer: 415953
day10a :: IO ()
day10a = do
    chunks :: [Chunk] <- day10_readInput "data/day10a_input.txt"

    putText "Chunks:"
    traverse_ print chunks
    putText ""

    print $ day10a_compute chunks

day10a_compute :: [Chunk] -> Int
day10a_compute !chunks =
    sum $
    mapMaybe checkChunkSyntax chunks

newtype Chunk = Chunk [ChunkChar]

instance Show Chunk where
    show (Chunk chars) = concatMap show chars

type SyntaxErrorScore = Int
type AutocompleteScore = Int

data ChunkType =
    Round
  | Square
  | Curly
  | Angle
    deriving (Eq, Show)

illegalClosingCharScore :: ChunkType -> SyntaxErrorScore
illegalClosingCharScore Round  = 3
illegalClosingCharScore Square = 57
illegalClosingCharScore Curly  = 1197
illegalClosingCharScore Angle  = 25137

data ChunkChar =
    Open  ChunkType
  | Close ChunkType

instance Show ChunkChar where
    show (Open Round)   = "("
    show (Open Square)  = "["
    show (Open Curly)   = "{"
    show (Open Angle)   = "<"
    show (Close Round)  = ")"
    show (Close Square) = "]"
    show (Close Curly)  = "}"
    show (Close Angle)  = ">"

toChunkChar :: Char -> ChunkChar
toChunkChar '(' = Open Round
toChunkChar '[' = Open Square
toChunkChar '{' = Open Curly
toChunkChar '<' = Open Angle
toChunkChar ')' = Close Round
toChunkChar ']' = Close Square
toChunkChar '}' = Close Curly
toChunkChar '>' = Close Angle
toChunkChar c   = panic $ "toChunkChar: invalid chunk char: " <> show c

-- | If the chunk is invalid, then return the score corresponding
-- to the first invalid char.
checkChunkSyntax :: Chunk -> Maybe SyntaxErrorScore
checkChunkSyntax (Chunk chars) =
    let
        checkChunk :: [ChunkType] -> [ChunkChar] -> Maybe SyntaxErrorScore
        checkChunk _openCharsStack [] = Nothing
        checkChunk openCharsStack (Open c : c_rest) =
            checkChunk (c : openCharsStack) c_rest
        checkChunk (s : s_rest) (Close c : c_rest) =
            if c == s then checkChunk s_rest c_rest
            else Just $ illegalClosingCharScore c
        checkChunk openCharsStack c_rest =
            panic $ "checkChunkSyntax: openCharsStack = " <> show openCharsStack <> ", chunk = " <> show c_rest
    in
        checkChunk [] chars

-- | If the chunk is incomplete, then return the score to autocomplete it.
-- Note that the chunk must not be corrupt.
checkChunkCompleteness :: Chunk -> Maybe AutocompleteScore
checkChunkCompleteness (Chunk chars) =
    let
        checkChunk :: [ChunkType] -> [ChunkChar] -> Maybe AutocompleteScore
        checkChunk [] [] = Nothing
        checkChunk openCharsStack [] =
            Just $ autocompleteScore openCharsStack
        checkChunk openCharsStack (Open c : c_rest) =
            checkChunk (c : openCharsStack) c_rest
        checkChunk (s : s_rest) (Close c : c_rest) =
            if c == s then checkChunk s_rest c_rest
            else panic "checkChunkCompleteness - corrupt chunk found"
        checkChunk openCharsStack c_rest =
            panic $ "checkChunkSyntax: openCharsStack = " <> show openCharsStack <> ", chunk = " <> show c_rest
    in
        checkChunk [] chars

autocompleteScore :: [ChunkType] -> AutocompleteScore
autocompleteScore chunkTypes =
    foldl' (\curScore ct -> 5 * curScore + chunkTypeAutoCompleteScore ct) 0 chunkTypes

chunkTypeAutoCompleteScore :: ChunkType -> AutocompleteScore
chunkTypeAutoCompleteScore Round  = 1
chunkTypeAutoCompleteScore Square = 2
chunkTypeAutoCompleteScore Curly  = 3
chunkTypeAutoCompleteScore Angle  = 4


day10_readInput :: FilePath -> IO [Chunk]
day10_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    pure $ map day10a_parseLine fileLines

day10a_parseLine :: Text -> Chunk
day10a_parseLine !txt =
    Chunk $
    map toChunkChar $
    T.unpack txt


-- | Answer: 2292863731
day10b :: IO ()
day10b = do
    chunks :: [Chunk] <- day10_readInput "data/day10a_input.txt"
    print $ day10b_compute chunks

day10b_compute :: [Chunk] -> Int
day10b_compute !chunks =
    let
        nonCorruptChunks :: [Chunk]
        nonCorruptChunks =
            filter (\c -> checkChunkSyntax c == Nothing) chunks

        autocompleteScores :: [AutocompleteScore]
        autocompleteScores =
            mapMaybe checkChunkCompleteness nonCorruptChunks
        
        medianScore :: AutocompleteScore
        medianScore =
            (sort autocompleteScores) !! (length autocompleteScores `div` 2)
    in
        medianScore
