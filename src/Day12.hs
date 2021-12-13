module Day12
    ( day12a
    , day12b
    ) where

import MyPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

import Utils


-- | Answer: 3463
day12a :: IO ()
day12a = do
    caveConnections :: [CaveConnection] <- day12_readInput "data/day12a_input.txt"

    putText "Cave Connections:"
    traverse_ print caveConnections
    putText ""

    print $ day12a_compute caveConnections

day12a_compute :: [CaveConnection] -> Int
day12a_compute !caveConnections =
    length $ pathsFromStartToEndA caveConnections

type Cave = Text

isBigCave :: Cave -> Bool
isBigCave !cave =
    T.all isUpper cave

startCave :: Cave
startCave = "start"

endCave :: Cave
endCave = "end"

type CavePath = [Cave]

data CaveConnection =
    CaveConnection {
        fromCave :: !Cave
      , toCave   :: !Cave
    }

instance Show CaveConnection where
    show CaveConnection{..} =
        T.unpack fromCave <> "-" <> T.unpack toCave

pathsFromStartToEndA :: [CaveConnection] -> [CavePath]
pathsFromStartToEndA !caveConnections =
    let
        connectedCavesMap :: Map Cave (Set Cave)
        connectedCavesMap =
            traceMap "connectedCavesMap = " $
            Map.fromListWith (<>) $
            map (Set.singleton <$>) $
            filter (\(_,c2) -> c2 /= startCave) $
            concatMap (\CaveConnection{..} -> [(fromCave, toCave), (toCave, fromCave)]) caveConnections

        connectedCaves :: Cave -> [Cave]
        connectedCaves !cave =
            maybe [] Set.toList $
            Map.lookup cave connectedCavesMap

        findPaths :: [CavePath] -> [CavePath]
        findPaths !curPaths =
            let
                extendedPaths :: [CavePath]
                extendedPaths = concatMap extendPath curPaths
            in
                if all (\p -> head p == Just endCave) extendedPaths then extendedPaths
                else findPaths extendedPaths

        extendPath :: CavePath -> [CavePath]
        extendPath [] = panic "unexpected: empty cave path"
        extendPath cavePath@(lastCave : _restPath)
            | lastCave == endCave = [cavePath]
            | otherwise =
                let
                    nextSteps :: [Cave]
                    nextSteps =
                        filter (\c -> isBigCave c || not (c `elem` cavePath)) $
                        connectedCaves lastCave
                in
                    map (\c -> c : cavePath) nextSteps
    in
        traceList "pathsFromStartToEndA = " $
        map reverse $
        findPaths [[startCave]]


pathsFromStartToEndB :: [CaveConnection] -> [CavePath]
pathsFromStartToEndB !caveConnections =
    let
        connectedCavesMap :: Map Cave (Set Cave)
        connectedCavesMap =
            traceMap "connectedCavesMap = " $
            Map.fromListWith (<>) $
            map (Set.singleton <$>) $
            filter (\(_,c2) -> c2 /= startCave) $
            concatMap (\CaveConnection{..} -> [(fromCave, toCave), (toCave, fromCave)]) caveConnections

        connectedCaves :: Cave -> [Cave]
        connectedCaves !cave =
            maybe [] Set.toList $
            Map.lookup cave connectedCavesMap

        findPaths :: [CavePath] -> [CavePath]
        findPaths !curPaths =
            let
                extendedPaths :: [CavePath]
                extendedPaths = concatMap extendPath curPaths
            in
                if all (\p -> head p == Just endCave) extendedPaths then extendedPaths
                else findPaths extendedPaths

        extendPath :: CavePath -> [CavePath]
        extendPath [] = panic "unexpected: empty cave path"
        extendPath cavePath@(lastCave : _restPath)
            | lastCave == endCave = [cavePath]
            | otherwise =
                let
                    smallCaveVisitedTwice :: Bool
                    smallCaveVisitedTwice =
                        let
                            checkCaves :: CavePath -> Bool
                            checkCaves [] = False
                            checkCaves (c:cs)
                                | isBigCave c = checkCaves cs
                                | otherwise = c `elem` cs || checkCaves cs
                        in
                            checkCaves cavePath

                    nextSteps :: [Cave]
                    nextSteps =
                        filter (\c -> isBigCave c 
                                      || not (c `elem` cavePath)
                                      || not smallCaveVisitedTwice
                               ) $
                        connectedCaves lastCave
                in
                    map (\c -> c : cavePath) nextSteps
    in
        traceList "pathsFromStartToEndB = " $
        map reverse $
        findPaths [[startCave]]

day12_readInput :: FilePath -> IO [CaveConnection]
day12_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    return $ mapMaybe day12a_parseLine fileLines

day12a_parseLine :: Text -> Maybe CaveConnection
day12a_parseLine !txt
    | [fromCave, toCave] <- T.splitOn "-" txt
    = Just CaveConnection{..}
    | otherwise
    = Nothing

-- | Answer: 91533
day12b :: IO ()
day12b = do
    caveConnections :: [CaveConnection] <- day12_readInput "data/day12a_input.txt"
    print $ day12b_compute caveConnections

day12b_compute :: [CaveConnection] -> Int
day12b_compute !caveConnections =
    length $ pathsFromStartToEndB caveConnections

