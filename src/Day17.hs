module Day17
    ( day17a
    , day17b
    ) where

import MyPrelude
-- import Data.List.NonEmpty ((<|))
-- import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- import qualified Data.Text as T
-- import qualified Prelude

import Utils


-- | Answer: 5050
day17a :: IO ()
day17a = do
    -- let target = Target 20 30 (-10) (-5)
    let target = Target 257 286 (-101) (-57)

    print $ day17a_compute target

day17a_compute :: Target -> Int
day17a_compute Target{..} =
    let
        vXOptions :: Map VelX [StepN]
        vXOptions = possibleXVelocities 50 xMin xMax

        vYOptions :: Map VelY [StepN]
        vYOptions = possibleYVelocities yMin yMax

        maxY :: Pos
        maxY = maxHeightForVy $ maximum $ Map.keys vYOptions
    in
        traceMap "vXOptions = " vXOptions `seq`
        traceMap "vYOptions = " vYOptions `seq`
        maxY

type Pos = Int
type VelX = Int
type VelY = Int
type StepN = Int

data Target =
    Target {
        xMin :: !Int
      , xMax :: !Int
      , yMin :: !Int
      , yMax :: !Int
    }

-- | Find the possible X velocities which will land within the X range of the target,
-- along with the number of steps for each.
possibleXVelocities :: StepN -> Int -> Int -> Map VelX [StepN]
possibleXVelocities !maxStepN !xMin !xMax =
    let
        vXResults :: VelX -> Maybe (VelX, [StepN])
        vXResults !vX =
            let
                checkSteps :: StepN -> Pos -> VelX -> [StepN]
                checkSteps !curStep !curX !curVx
                    | curStep > maxStepN = [] 
                    | curX > xMax = []
                    | curX >= xMin && curX <= xMax
                    = curStep : checkSteps (curStep + 1) (curX + curVx) (max 0 $ curVx - 1)
                    | curVx == 0 = []
                    | otherwise = checkSteps (curStep + 1) (curX + curVx) (max 0 $ curVx - 1)

                steps :: [StepN]
                steps =
                    -- take 10 $
                    checkSteps 0 0 vX
            in do
                guard $ not $ null steps
                pure (vX, steps)
    in
        Map.fromList $
        mapMaybe vXResults [1 .. xMax]


possibleYVelocities :: Int -> Int -> Map VelY [StepN]
possibleYVelocities !yMin !yMax =
    let
        vYResults :: VelY -> Maybe (VelY, [StepN])
        vYResults !vY =
            let
                checkSteps :: StepN -> Pos -> VelY -> [StepN]
                checkSteps !curStep !curY !curVy
                    | curY < yMin = []
                    | curY >= yMin && curY <= yMax
                    = curStep : checkSteps (curStep + 1) (curY + curVy) (curVy - 1)
                    | otherwise = checkSteps (curStep + 1) (curY + curVy) (curVy - 1)

                steps :: [StepN]
                steps = checkSteps 0 0 vY
            in do
                guard $ not $ null steps
                pure (vY, steps)
    in
        Map.fromList $
        mapMaybe vYResults [yMin .. 1000]

maxHeightForVy :: VelY -> Pos
maxHeightForVy !vY
    | vY <= 0 = 0
    | otherwise = sum [1..vY]


-- | Answer: 2223
day17b :: IO ()
day17b = do
    -- let target = Target 20 30 (-10) (-5)
    let target = Target 257 286 (-101) (-57)

    print $ day17b_compute target

day17b_compute :: Target -> Int
day17b_compute Target{..} =
    let
        vYOptions :: Map VelY [StepN]
        vYOptions = possibleYVelocities yMin yMax

        maxStepN :: StepN
        maxStepN = maximum $ mconcat $ Map.elems vYOptions

        vXOptions :: Map VelX [StepN]
        vXOptions = possibleXVelocities maxStepN xMin xMax

        xVelsByStep :: Map StepN (Set VelX)
        xVelsByStep =
            Map.fromListWith (<>) $
            concatMap (\(vX, steps) -> (, Set.singleton vX) <$> steps) $
            Map.toList vXOptions

        yVelsByStep :: Map StepN (Set VelY)
        yVelsByStep =
            Map.fromListWith (<>) $
            concatMap (\(vY, steps) -> (, Set.singleton vY) <$> steps) $
            Map.toList vYOptions

        velsByStep :: Map StepN [(VelX, VelY)]
        velsByStep =
            Map.intersectionWith (\xs ys -> do
                                    vX <- Set.toList xs
                                    vY <- Set.toList ys
                                    pure (vX, vY)
                                 )
                                 xVelsByStep
                                 yVelsByStep

        allVels :: Set (VelX, VelY)
        allVels =
            Set.fromList $
            mconcat $
            Map.elems velsByStep
    in
        traceMap "vXOptions = " vXOptions `seq`
        traceMap "vYOptions = " vYOptions `seq`
        traceMap "xVelsByStep = " xVelsByStep `seq`
        traceMap "yVelsByStep = " yVelsByStep `seq`
        traceMap "velsByStep = " velsByStep `seq`
        traceSet "allVels = " allVels `seq`
        Set.size allVels

