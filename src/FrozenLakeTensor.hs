{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies  #-}

module FrozenLakeTensor where

import Control.Monad.State
import qualified Data.Array as A
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Float (double2Float)
import qualified System.Random as Rand
import TensorFlow.Core
import TensorFlow.Minimize
import TensorFlow.Ops hiding (initializedVariable)
import TensorFlow.Session
import TensorFlow.Variable

import Environment

data TileType =
  Start |
  Goal |
  Frozen |
  Hole
  deriving (Show, Eq)

charToTile :: Char -> TileType
charToTile 'S' = Start
charToTile 'G' = Goal
charToTile 'F' = Frozen
charToTile 'H' = Hole

type FrozenLakeObservation = Word

data FrozenLakeAction =
  MoveLeft |
  MoveDown |
  MoveRight |
  MoveUp
  deriving (Show, Eq, Enum)

data FrozenLakeEnvironment = FrozenLakeEnvironment
  { currentObs :: FrozenLakeObservation
  , grid :: A.Array Word TileType
  , slipChance :: Double -- 0 to 1
  , randomGenerator :: Rand.StdGen
  , previousAction :: Maybe FrozenLakeAction
  , dimens :: (Word, Word) -- Rows, Cols
  , flExplorationRate :: Double
  }

-- Build Model
-- Choose Action
-- Learn from the environment

data Model = Model
  { weightsT :: Variable Float
  , chooseActionStep :: TensorData Float -> Session (Vector Float)
  , learnStep :: TensorData Float -> TensorData Float -> Session ()
  }

createModel :: Session Model
createModel = do
  -- Choose Action
  (inputs :: Tensor Value Float) <- placeholder (Shape [1, 16])
  (weights :: Variable Float) <- truncatedNormal (vector [16, 4]) >>= initializedVariable
  let (results :: Tensor Build Float) = (inputs `matMul` readValue weights)
  (returnedOutputs :: Tensor Value Float) <- render results

  -- Train Nextwork
  (nextOutputs :: Tensor Value Float) <- placeholder (Shape [4, 1])
  let (diff :: Tensor Build Float) = nextOutputs `sub` results
  let (loss :: Tensor Build Float) = reduceSum (diff `mul` diff)
  trainer_ <- minimizeWith adam loss [weights]
  let chooseStep = \inputFeed -> runWithFeeds [feed inputs inputFeed] returnedOutputs
  let trainStep = \inputFeed nextOutputFeed ->
        runWithFeeds [feed inputs inputFeed, feed nextOutputs nextOutputFeed] trainer_
  return $ Model weights chooseStep trainStep

resetFrozenLake :: FrozenLake FrozenLakeObservation
resetFrozenLake = FrozenLake $ do
  let initialObservation = 0
  (fle, model) <- get
  put $ (fle { currentObs = initialObservation, previousAction = Nothing }, model)
  return initialObservation

stepFrozenLake :: FrozenLakeAction -> FrozenLake (FrozenLakeObservation, Reward, Bool)
stepFrozenLake act = do
  fle <- get
  let obs = currentObs fle
  let (slipRoll, gen') = Rand.randomR (0.0, 1.0) (randomGenerator fle)
  let allLegalMoves = legalMoves obs (dimens fle)
  let (randomMoveIndex, finalGen) = Rand.randomR (0, length allLegalMoves - 1) gen'
  let newObservation = if slipRoll >= slipChance fle
        then if act `elem` allLegalMoves
          then applyMoveUnbounded act obs (snd . dimens $ fle)
          else obs
        else applyMoveUnbounded (allLegalMoves !! randomMoveIndex) obs (snd . dimens $ fle)
  let (done, reward) = case (grid fle) A.! newObservation of
        Goal -> (True, Reward 1.0)
        Hole -> (True, Reward 0.0)
        _ -> (False, Reward 0.0)
  put $ fle {currentObs = newObservation, randomGenerator = finalGen, previousAction = Just act}
  return (newObservation, reward, done)

basicEnv :: IO FrozenLakeEnvironment
basicEnv = do
  gen <- Rand.getStdGen
  return $ FrozenLakeEnvironment
    { currentObs = 0
    , grid = A.listArray (0, 15) (charToTile <$> "SFFFFHFHFFFHHFFG")
    , slipChance = 0.0
    , randomGenerator = gen
    , previousAction = Nothing
    , dimens = (4, 4)
    , flExplorationRate = 0.9
    }

obsToTensor :: FrozenLakeObservation -> TensorData Float
obsToTensor obs = encodeTensorData (Shape [1, 16]) (V.fromList asList)
  where
    asList = replicate (fromIntegral obs) 0.0 ++ [1.0] ++ replicate (fromIntegral (15 - obs)) 0.0

chooseActionTensor :: FrozenLake FrozenLakeAction
chooseActionTensor = FrozenLake $ do
  (fle, model) <- get
  let (exploreRoll, gen') = Rand.randomR (0.0, 1.0) (randomGenerator fle)
  if exploreRoll < flExplorationRate fle
    then do
      let (actionRoll, gen'') = Rand.randomR (0, 3) gen'
      put $ (fle { randomGenerator = gen'' }, model)
      return (toEnum actionRoll)
    else do
      let obs1 = currentObs fle
      let obs1Data = obsToTensor obs1
      (results :: Vector Float) <- lift ((chooseActionStep model) obs1Data)
      let bestMoveIndex = V.maxIndex results
      put $ (fle { randomGenerator = gen' }, model)
      return (toEnum bestMoveIndex)

learnTensor ::
  FrozenLakeObservation -> FrozenLakeObservation ->
  Reward -> FrozenLakeAction ->
  FrozenLake ()
learnTensor obs1 obs2 (Reward reward) action = FrozenLake $ do
  model <- snd <$> get
  let obs1Data = obsToTensor obs1
  (results :: Vector Float) <- lift ((chooseActionStep model) obs1Data)
  let (bestMoveIndex, maxScore) = (V.maxIndex results, V.maximum results)
  let targetActionValues = results V.// [(bestMoveIndex, double2Float reward + (gamma * maxScore))]
  let obs2Data = obsToTensor obs2
  let targetActionData = encodeTensorData (Shape [4, 1]) targetActionValues
  lift $ (learnStep model) obs2Data targetActionData
  where
    gamma = 0.81

newtype FrozenLake a = FrozenLake
  (StateT (FrozenLakeEnvironment, Model) Session a)
  deriving (Functor, Applicative, Monad)

instance (MonadState FrozenLakeEnvironment) FrozenLake where
  get = FrozenLake (fst <$> get)
  put fle = FrozenLake $ do
    (_, model) <- get
    put (fle, model)

instance EnvironmentMonad FrozenLake where
  type (Observation FrozenLake) = FrozenLakeObservation
  type (Action FrozenLake) = FrozenLakeAction
  type (EnvironmentState FrozenLake) = FrozenLakeEnvironment
  baseEnv = basicEnv
  currentObservation = currentObs <$> get
  resetEnv = resetFrozenLake
  stepEnv = stepFrozenLake
  runEnv env (FrozenLake action) = runSession $ do
    model <- createModel
    evalStateT action (env, model)

instance LearningEnvironment FrozenLake where
  chooseActionBrain = chooseActionTensor
  learnEnv = learnTensor
  explorationRate = flExplorationRate <$> get
  reduceExploration decayRate minEpsilon = do
    fle <- get
    let e = flExplorationRate fle
    let newE = max minEpsilon (e * decayRate)
    put $ fle { flExplorationRate = newE }

legalMoves :: FrozenLakeObservation -> (Word, Word) -> [FrozenLakeAction]
legalMoves observation (numRows, numCols) = catMaybes [left, down, right, up]
  where
    (row, col) = quotRem observation numRows
    left = if col > 0 then Just MoveLeft else Nothing
    down = if row < numRows - 1 then Just MoveDown else Nothing
    right = if col < numCols - 1 then Just MoveRight else Nothing
    up = if row > 0 then Just MoveUp else Nothing

-- Does NOT do bounds checking
applyMoveUnbounded :: FrozenLakeAction -> FrozenLakeObservation -> Word -> FrozenLakeObservation
applyMoveUnbounded action obs numCols = case action of
  MoveLeft -> obs - 1 
  MoveDown -> obs + numCols
  MoveRight -> obs + 1
  MoveUp -> obs - numCols
