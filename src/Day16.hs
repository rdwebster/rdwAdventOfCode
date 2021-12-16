module Day16
    ( day16a
    , day16b
    ) where

import MyPrelude
import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Prelude

-- import Utils


-- | Answer: 923
day16a :: IO ()
day16a = do
    hex :: Hex <- day16_readInput "data/day16a_input.txt"

    putText "Hex:"
    putText hex
    putText ""

    let bits :: TBits
        bits = hexToBits hex

    putText "Bits:"
    print bits
    putText ""

    print $ day16a_compute hex

day16a_compute :: Hex -> Int
day16a_compute !hex =
    either (panic . show) sumPacketVersions $
    P.runParser packetParser "packet" $
    hexToBits hex


type Hex     = Text
type TBits   = [TBit]
type Version = Int
type TypeID  = Int

data TBit = Zero | One
    deriving (Eq, Ord)

instance Show TBit where
    show Zero = "0"
    show One  = "1"

hexDigitBits :: Map Char TBits
hexDigitBits =
    Map.fromList [
          ('0', [Zero, Zero, Zero, Zero])
        , ('1', [Zero, Zero, Zero, One ])
        , ('2', [Zero, Zero, One , Zero])
        , ('3', [Zero, Zero, One , One ])
        , ('4', [Zero, One , Zero, Zero])
        , ('5', [Zero, One , Zero, One ])
        , ('6', [Zero, One , One , Zero])
        , ('7', [Zero, One , One , One ])
        , ('8', [One , Zero, Zero, Zero])
        , ('9', [One , Zero, Zero, One ])
        , ('A', [One , Zero, One , Zero])
        , ('B', [One , Zero, One , One ])
        , ('C', [One , One , Zero, Zero])
        , ('D', [One , One , Zero, One ])
        , ('E', [One , One , One , Zero])
        , ('F', [One , One , One , One ])
        ]

hexToBits :: Hex -> TBits
hexToBits !hex =
    concatMap (\c -> fromMaybe (panic "unexpected - failed to look u ") $ Map.lookup c hexDigitBits) $
    T.unpack hex

data Packet =
    LiteralPacket {
        version    :: !Version
      , typeID     :: !TypeID
      , literalVal :: !Int
    } |
    OperatorPacket {
        version    :: !Version
      , typeID     :: !TypeID
      , subPackets :: ![Packet]
    }
    deriving (Show)

sumPacketVersions :: Packet -> Version
sumPacketVersions LiteralPacket{..}  = version
sumPacketVersions OperatorPacket{..} = version + sum (map sumPacketVersions subPackets)

evaluatePacket :: Packet -> Int
evaluatePacket LiteralPacket{..} = literalVal
evaluatePacket OperatorPacket{..}
    | typeID == 0   -- Sum
    = sum $ evaluatePacket <$> subPackets
    | typeID == 1   -- Product
    = product $ evaluatePacket <$> subPackets
    | typeID == 2   -- Minimum
    = minimum $ evaluatePacket <$> subPackets
    | typeID == 3   -- Maximum
    = maximum $ evaluatePacket <$> subPackets
    | typeID == 5   -- GreaterThan
    , [p1, p2] <- evaluatePacket <$> subPackets
    = if p1 > p2 then 1 else 0
    | typeID == 6   -- LessThan
    , [p1, p2] <- evaluatePacket <$> subPackets
    = if p1 < p2 then 1 else 0
    | typeID == 7   -- Equal
    , [p1, p2] <- evaluatePacket <$> subPackets
    = if p1 == p2 then 1 else 0
    | otherwise = panic "unexpected TypeID"


bitsToNumber :: TBits -> Int
bitsToNumber !bits =
    foldl' (\curVal b -> curVal * 2 + bitVal b) 0 bits

bitVal :: TBit -> Int
bitVal Zero = 0
bitVal One  = 1


type Parser = P.Parsec Void TBits

bitNumParserN :: Int -> Parser Int
bitNumParserN !nBits = do
    numBits :: TBits <- P.takeP (Just "bit") nBits
    pure $ bitsToNumber numBits

packetParser :: Parser Packet
packetParser = do
    version <- bitNumParserN 3
    typeID  <- bitNumParserN 3

    if typeID == 4
        then do
            literalVal <- literalNumParser
            pure LiteralPacket{..}
        else do
            lengthTypeID :: Int <- bitNumParserN 1
            case lengthTypeID of
                0 -> do
                    subPacketLen  :: Int <- bitNumParserN 15
                    subPacketBits :: TBits <- P.takeP (Just "bit") subPacketLen
                    let subPackets :: [Packet] = either (panic . show) identity $ P.runParser (many packetParser) "subpacket" subPacketBits
                    pure OperatorPacket{..}
                1 -> do
                    numOfSubPackets :: Int <- bitNumParserN 11
                    subPackets :: [Packet] <- P.count numOfSubPackets packetParser
                    pure OperatorPacket{..}
                _ ->
                    panic "unexpected value of lengthTypeID"

literalNumParser :: Parser Int
literalNumParser =
    bitsToNumber <$> literalNumBitsParser

literalNumBitsParser :: Parser TBits
literalNumBitsParser = do
    continue  <- bitNumParserN 1
    chunkBits <- P.takeP (Just "bit") 4
    if continue == 0
        then pure chunkBits
        else (chunkBits <>) <$> literalNumBitsParser


day16_readInput :: FilePath -> IO Hex
day16_readInput !fileName = do
    fileTxt <- readFile fileName
    let fileLines :: [Text] = lines fileTxt
    pure $ unsafeHead fileLines


-- | Answer: 258888628940
day16b :: IO ()
day16b = do
    hex :: Hex <- day16_readInput "data/day16a_input.txt"

    putText "Hex:"
    putText hex
    putText ""

    print $ day16b_compute hex

day16b_compute :: Hex -> Int
day16b_compute !hex =
    either (panic . show) evaluatePacket $
    P.runParser packetParser "packet" $
    hexToBits hex

