-- | Implementation of an IntCode computer.
module IntCode
    ( Name
    , Input
    , Output
    , Memory
    , ProgramState
    , extractInputs
    , extractOutputs
    , makeProgramState
    , makeProgramState'
    , readProgram
    , runProgram
    , setMemValueAt
    , setProgramInputDefault
    ) where

import MyPrelude
import qualified Prelude
import Control.Concurrent.STM
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- import Utils


type Name   = Text
type Input  = Int
type Output = Int


data ProgramState =
    ProgramState {
        ps_name         :: !Name
      , ps_address      :: !Addr
      , ps_relBase      :: !Addr
      , ps_memory       :: !Memory
      , ps_inputs       :: !(TQueue Input)
      , ps_inputDefault :: !(Maybe Input)   -- If specified, use this when no input is available (instead of waiting).
      , ps_outputs      :: !(TQueue Output)
    }

instance Prelude.Show ProgramState where
    show ProgramState{..} =
        mconcat [
            T.unpack ps_name
          , "\n    Address: "
          , show ps_address
          , "\n    Relative Base: "
          , show ps_relBase
          , "\n    Memory: "
          , show ps_memory
          -- , "\nInputs: "
          -- , show ps_inputs
          -- , "\nOutputs: "
          -- , show ps_outputs
        ]

makeProgramState :: Name -> Memory -> [Input] -> IO ProgramState
makeProgramState !name !memory !inputVals = do
    inputQueue <- atomically $ do
        q <- newTQueue
        mapM_ (writeTQueue q) inputVals
        return q

    outputQueue <- newTQueueIO

    pure $ ProgramState{
                ps_name         = name
              , ps_address      = 0
              , ps_relBase      = 0
              , ps_memory       = memory
              , ps_inputs       = inputQueue
              , ps_inputDefault = Nothing
              , ps_outputs      = outputQueue
            }

makeProgramState' :: Name -> Memory -> TQueue Input -> TQueue Output -> ProgramState
makeProgramState' !name !memory !inputQueue !outputQueue =
    ProgramState{
        ps_name         = name
      , ps_address      = 0
      , ps_relBase      = 0
      , ps_memory       = memory
      , ps_inputs       = inputQueue
      , ps_inputDefault = Nothing
      , ps_outputs      = outputQueue
    }

setProgramInputDefault :: Input -> ProgramState -> ProgramState
setProgramInputDefault !defInput !ps =
    ps{ ps_inputDefault = Just defInput }

newtype Memory =
    Memory {
        memAddrValues :: (IntMap Int)
    }

instance Prelude.Show Memory where
    show Memory{..} = show $ IntMap.elems memAddrValues

makeMemory :: [Int] -> Memory
makeMemory !vals =
    Memory $ IntMap.fromList $ zip [0..] vals


type OpCode = Int
type Val    = Int
type Addr   = Int

data ParamMode =
    PositionMode    -- ^ Parameter is an address pointing to the value.
  | ImmediateMode   -- ^ Paremeter is the value itself.
  | RelativeMode    -- ^ Parameter is an offset from the current Relative Base address.
    deriving (Eq, Ord, Show)


memValueAt :: Memory -> Addr -> Val
memValueAt Memory{memAddrValues} !addr =
    -- memAddrValues IntMap.! addr
    IntMap.findWithDefault 0 addr memAddrValues

setMemValueAt :: Memory -> Addr -> Val -> Memory
setMemValueAt Memory{memAddrValues} !addr !newCode =
    Memory $
    IntMap.insert addr newCode memAddrValues 

runProgram :: ProgramState -> IO ProgramState
runProgram !ps = do
    maybeNewState <- runInstruction ps
    case maybeNewState of
        Nothing -> pure ps
        Just newState -> runProgram newState

-- | Extract any outputs from the program state.
extractOutputs :: ProgramState -> IO [Output]
extractOutputs ps@ProgramState{ps_outputs} = do
    maybeOutput <- atomically $ tryReadTQueue ps_outputs
    case maybeOutput of
        Nothing -> pure []
        Just outVal -> (outVal :) <$> extractOutputs ps

-- | Extract any inputs from the program state.
extractInputs :: ProgramState -> IO [Input]
extractInputs ps@ProgramState{ps_inputs} = do
    maybeInput <- atomically $ tryReadTQueue ps_inputs
    case maybeInput of
        Nothing -> pure []
        Just inVal -> (inVal :) <$> extractInputs ps
    

-- | Runs one step of instruction at the given offset.
-- Returns Nothing if the program should terminate.
runInstruction :: ProgramState -> IO (Maybe ProgramState)
runInstruction !ps =
    let
        opCode :: OpCode
        paramModes :: [ParamMode]
        (opCode, paramModes) =
            -- traceVal "(opCode, paramModes) = " $
            currentOpCodeAndParams ps

        operand1 :: Val
        operand1 =
            -- traceVal "operand1 = " $
            operandValue ps 0 $ paramModes !! 0

        operand2 :: Val
        operand2 =
            -- traceVal "operand2 = " $
            operandValue ps 1 $ paramModes !! 1

        opAddr1 :: Addr
        opAddr1 =
            operandAddr ps 0 $ paramModes !! 0

        opAddr3 :: Addr
        opAddr3 =
            operandAddr ps 2 $ paramModes !! 2
    in
        case opCode of
        99 ->
            -- traceVal ("\n" <> T.unpack (ps_name ps) <> ":  END") () `seq`
            pure Nothing
        1  ->
            -- Addition
            pure $
            -- traceVal ("\n" <> "add result = ") $
            Just $ advanceAddress 3 $ storeResult ps opAddr3 $ operand1 + operand2

        2  ->
            -- Multiplication
            pure $
            -- traceVal ("\n" <> "mult result = ") $
            Just $ advanceAddress 3 $ storeResult ps opAddr3 $ operand1 * operand2

        3  -> do
            -- Store input
            newPS <- storeInput ps opAddr1
            pure $
                -- traceVal ("\n" <> "stored input  = ") $
                Just $ advanceAddress 1 newPS

        4  -> do
            -- Store output
            newPS <- recordOutput ps operand1
            pure $ 
                -- traceVal ("\n" <> "stored output  = ") $
                Just $ advanceAddress 1 newPS

        5  ->
            -- Jump if true
            pure $ 
            -- traceVal ("\n" <> "jump if true = ") $
            if operand1 /= 0 then
                Just $ setAddress operand2 ps
            else
                Just $ advanceAddress 2 ps 

        6  ->
            -- Jump if false
            pure $
            -- traceVal ("\n" <> "jump if false = ") $
            if operand1 == 0 then
                Just $ setAddress operand2 ps
            else
                Just $ advanceAddress 2 ps 

        7  ->
            -- Less than
            pure $ 
            -- traceVal ("\n" <> "less than = ") $
            Just $ advanceAddress 3 $ storeResult ps opAddr3 $ if operand1 < operand2 then 1 else 0

        8  ->
            -- Equals
            pure $ 
            -- traceVal ("\n" <> "equals = ") $
            Just $ advanceAddress 3 $ storeResult ps opAddr3 $ if operand1 == operand2 then 1 else 0

        9 ->
            -- Adjust the relative base
            pure $
            -- traceVal ("\n" <> "adjust relative base = ") $
            Just $ advanceAddress 1 $ adjustRelativeBase operand1 ps

        otherCode ->
            -- traceVal ("\n" <> "unexpected op code: " <> show otherCode) () `seq`
            panic $ "unexpected op code: " <> show otherCode

-- | Extracts the opCode value from the current address, as well as
-- the parameter modes.
currentOpCodeAndParams :: ProgramState -> (OpCode, [ParamMode])
currentOpCodeAndParams ProgramState{..} =
    let
        curVal :: Val
        curVal = memValueAt ps_memory ps_address

        -- The OpCode is contained in the last 2 digits of the number.
        opCode :: OpCode
        opCode = curVal `mod` 100

        paramModes :: [ParamMode]
        paramModes = extractParamModes $ curVal `div` 100
    in
        (opCode, paramModes)
    
extractParamModes :: Val -> [ParamMode]
extractParamModes !v =
    let
        nextParamMode :: ParamMode
        nextParamMode =
            case v `mod` 10 of
            0 -> PositionMode
            1 -> ImmediateMode
            2 -> RelativeMode
            otherVal -> panic $ "unexpected param mode value:  " <> show otherVal
    in
        nextParamMode : extractParamModes (v `div` 10)

-- | Fetch the nth operand value.
operandValue :: ProgramState -> Int -> ParamMode -> Val
operandValue ProgramState{..} !operandN !paramMode =
    let
        val :: Val
        val = memValueAt ps_memory $ ps_address + operandN + 1
    in
        case paramMode of
        PositionMode  -> memValueAt ps_memory val
        ImmediateMode -> val
        RelativeMode  -> memValueAt ps_memory $ ps_relBase + val

-- | Fetch the address corresponding to the nth operand.
operandAddr :: ProgramState -> Int -> ParamMode -> Addr
operandAddr ProgramState{..} !operandN !paramMode =
    let
        val :: Val
        val = memValueAt ps_memory $ ps_address + operandN + 1
    in
        case paramMode of
        PositionMode  -> val
        ImmediateMode -> panic "Cannot write to a value in Immediate Mode"
        RelativeMode  -> ps_relBase + val

storeResult :: ProgramState -> Addr -> Val -> ProgramState
storeResult ps@ProgramState{..} !resultAddr !val =
    ps{ps_memory = setMemValueAt ps_memory resultAddr val}


advanceAddress :: Int -> ProgramState -> ProgramState
advanceAddress !nOperands ps@ProgramState{ps_address} =
    ps{ps_address = ps_address + nOperands + 1}

setAddress :: Addr -> ProgramState -> ProgramState
setAddress !newAddr !ps =
    ps{ps_address = newAddr}

adjustRelativeBase :: Int -> ProgramState -> ProgramState
adjustRelativeBase operand1 ps@ProgramState{ps_relBase} =
    ps{ps_relBase = ps_relBase + operand1}

recordOutput :: ProgramState -> Val -> IO ProgramState
recordOutput ps@ProgramState{ps_outputs} !outputVal = do
    atomically $ writeTQueue ps_outputs outputVal
    pure ps

-- | Take the first input and store it in the address specified.
storeInput :: ProgramState -> Addr -> IO ProgramState
storeInput ps@ProgramState{ps_inputs, ps_inputDefault} !storeAddr =
    case ps_inputDefault of
    Nothing -> atomically $ do
        i <- readTQueue ps_inputs
        pure $ storeResult ps storeAddr i
    -- If a default input is specified, then use this immediately when no value is available in the input queue.
    Just inputDef -> do
        threadDelay 1000        -- Wait a small amount of time for values to appear.
        atomically $ do
            i <- tryReadTQueue ps_inputs
            pure $ storeResult ps storeAddr $ fromMaybe inputDef i

-- | Loads an IntCode program memory from file.
readProgram :: FilePath -> IO Memory
readProgram !fileName = do
    fileTxt <- TIO.readFile fileName
    let vals = T.splitOn "," fileTxt
    return $ makeMemory $ mapMaybe (readMaybe . T.unpack) vals


