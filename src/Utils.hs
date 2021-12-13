module Utils
    ( fst3, snd3, thd3
    , mapIndexed
    , mapJoin
    , textToInt
    , traceVal
    , traceList
    , traceSet
    , traceMap
    , traceIntMap
    , traceWithArg
    , traceWithArgs2
    , traceWithArgs3
    ) where

import MyPrelude
-- import Data.Char (isSpace)
import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (trace)


-- | Parsers an integer value from Text.
textToInt :: Text -> Maybe Int
textToInt !txt =
    readMaybe $ T.unpack txt


-- | Concatenates lists of values (or Strings) produced by a mapping function with the specified separator between each.
--   Example: mapJoin ", " show [1..9] = "1, 2, 3, 4, 5, 6, 7, 8, 9"
mapJoin :: Foldable t => [b] -> (a -> [b]) -> t a -> [b]
mapJoin sep fn vals =
    List.intercalate sep $ map fn $ Foldable.toList vals


-- | Applies a function to each element of the list, where the function is passed
--   both the element value and its zero-based index in the list.
mapIndexed :: (a -> Int -> b) -> [a] -> [b]
mapIndexed mapFunction ls =
    let
        mapIndexedHelper _ [] = []
        mapIndexedHelper index (x:xs) = mapFunction x index : mapIndexedHelper (index + 1) xs
    in
        mapIndexedHelper 0 ls


-- | Truncates the string to fit within the specific length, if necesary.
-- If truncated, the string will end in '...', with the final length being the maximum allowed length.
truncateWithElipsis :: Int -> String -> String
truncateWithElipsis !maxLen !str =
    if length str > maxLen then
        take (maxLen - 3) str <> "..."
    else
        str

-- | Replaces sequences of whitespace (including new-lines) with single spaces.
consolidateWhitespace :: String -> String
consolidateWhitespace [] = []
consolidateWhitespace (headChar : tailChars) =
    if isSpace headChar then
        ' ' : consolidateWhitespace (dropWhile isSpace tailChars)
    else headChar : consolidateWhitespace tailChars


-- | Traces the specified showable value and returns the value.
-- | @arg message    a message to be included with each trace output, followed by the Show form of the value
-- | @arg val        the value to be traced and returned
-- | @return         the original value after tracing it
-- |
traceVal :: Show a => String -> a -> a
traceVal = traceValWith show


-- | Traces the specified value (using the show function provided) and returns its original value.
-- | @arg showValFn  a function to produce the string output for the value
-- | @arg message    a message to be included with each trace output, followed by the Show form of the value
-- | @arg val        the value to be traced and returned
-- | @return         the original value after tracing it
-- | @see trace
-- |
traceValWith :: (a -> String) -> String -> a -> a
traceValWith !showValFn !message !val = trace (message <> showValFn val) val


-- | Traces the specified showable list of values -- each item on its own line -- and returns the original list.
-- | @arg message  a message to be included with each trace output, followed by the Show form of the list values
-- | @arg lst      the list of values to be traced and returned
-- | @return       the original list of values after tracing it
-- |
traceList :: Show a => String -> [a] -> [a]
traceList !message [] = trace (message <> "<nil>") []
traceList message lst = trace (message <> "\n    " <> mapJoin "\n    " show lst) lst


-- | Traces the specified showable set of values -- each item on its own line -- and returns the original set.
-- | @arg message  a message to be included with each trace output, followed by the Show form of the set values
-- | @arg set      the set of values to be traced and returned
-- | @return       the original set of values after tracing it
-- |
traceSet :: Show a => String -> Set a -> Set a
traceSet !message !set =
    if Set.null set then trace (message <> "<nil>") set
    else trace (message <> "\n    " <> mapJoin "\n    " show (Set.toList set)) set


-- | Traces the specified showable map of values -- each item on its own line -- and returns the original map.
-- | @arg message  a message to be included with each trace output, followed by the Show form of the map values
-- | @arg map      the map of values to be traced and returned
-- | @return       the original map of values after tracing it
-- |
traceMap :: (Show a, Show b) => String -> Map a b -> Map a b
traceMap !message !m =
    if Map.null m then trace (message <> "<nil>") m
    else trace (message <> "\n    " <> mapJoin "\n    " (\(k, v) -> show k <> " -> " <> show v) (Map.toList m)) m

traceIntMap :: (Show a) => String -> IntMap a -> IntMap a
traceIntMap !message !m =
    if IntMap.null m then trace (message <> "<nil>") m
    else trace (message <> "\n    " <> mapJoin "\n    " (\(k, v) -> show k <> " -> " <> show v) (IntMap.toList m)) m


-- | Traces the specified value before returning it.
-- | The argument value will be combined with the message, such that the output looks like:
-- |   message(argText) = valText
-- |
traceWithArg :: (Show a, Show b) => String -> a -> b -> b
traceWithArg !message !arg =
    traceVal (message <> "(" <> showArg arg <> ") = ")

-- | Traces the specified value before returning it.
-- | The 2 argument values will be combined with the message, such that the output looks like:
-- |   message(arg1Text, arg2Text) = valText
-- |
traceWithArgs2 :: (Show a, Show b, Show c) => String -> a -> b -> c -> c
traceWithArgs2 !message !arg1 !arg2 =
    traceVal (message <> "(" <> showArg arg1 <> ", " <> showArg arg2 <> ") = ")

-- | Traces the specified value before returning it.
-- | The 3 argument values will be combined with the message, such that the output looks like:
-- |   message(arg1Text, arg2Text, arg3Text) = valText
-- |
traceWithArgs3 :: (Show a, Show b, Show c, Show d) => String -> a -> b -> c -> d -> d
traceWithArgs3 !message !arg1 !arg2 !arg3 =
    traceVal (message <> "(" <> showArg arg1 <> ", " <> showArg arg2 <> ", " <> showArg arg3 <> ") = ")


showArg :: Show a => a -> String
showArg !arg =
    -- Replace any new lines with spaces.
    -- Limit the value to 100 chars.
    truncateWithElipsis 100 $
    consolidateWhitespace $
    show arg

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

