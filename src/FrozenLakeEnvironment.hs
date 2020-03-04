{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module FrozenLakeEnvironment where

import Control.Monad (forM_)
import Control.Monad.State
import qualified Data.Array as A
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import qualified System.Random as Rand

import Environment

data TileType =
  Start |
  Goal |
  Frozen |
  Hole
  deriving (Show, Eq)

isSafe :: TileType -> Bool
isSafe Hole = False
isSafe _ = True

tileToChar :: TileType -> Char
tileToChar Start = 'S'
tileToChar Goal = 'G'
tileToChar Frozen = 'F'
tileToChar Hole = 'H'

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
  , qTable :: A.Array (Word, Word) Double
  , flExplorationRate :: Double
  }

resetFrozenLake :: FrozenLake FrozenLakeObservation
resetFrozenLake = FrozenLake $ do
  let initialObservation = 0
  fle <- get
  put $ fle { currentObs = initialObservation, previousAction = Nothing }
  return initialObservation

stepFrozenLake :: FrozenLakeAction -> FrozenLake (FrozenLakeObservation, Reward, Bool)
stepFrozenLake act = FrozenLake $ do
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

-- Does NOT do bounds checking
applyMoveUnbounded :: FrozenLakeAction -> FrozenLakeObservation -> Word -> FrozenLakeObservation
applyMoveUnbounded action obs numCols = case action of
  MoveLeft -> obs - 1 
  MoveDown -> obs + numCols
  MoveRight -> obs + 1
  MoveUp -> obs - numCols

legalMoves :: FrozenLakeObservation -> (Word, Word) -> [FrozenLakeAction]
legalMoves observation (numRows, numCols) = catMaybes [left, down, right, up]
  where
    (row, col) = quotRem observation numRows
    left = if col > 0 then Just MoveLeft else Nothing
    down = if row < numRows - 1 then Just MoveDown else Nothing
    right = if col < numCols - 1 then Just MoveRight else Nothing
    up = if row > 0 then Just MoveUp else Nothing

renderFrozenLake :: FrozenLake ()
renderFrozenLake = FrozenLake $ do
  fle <- get
  let obs = currentObs fle
  liftIO $ do
    let elements = A.assocs (grid fle)
        numCols = fromIntegral . snd . dimens $ fle
        rows = chunksOf numCols elements
    putStrLn $ case (previousAction fle) of
      Nothing -> ""
      Just a -> "    " ++ show a
    forM_ rows (renderRow obs)
  where
    renderRow obs row = do
      forM_ row (\(idx, t) -> liftIO $ if idx == obs
        then liftIO $ putChar 'X'
        else liftIO $ putChar (tileToChar t))
      putChar '\n'

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
    , qTable = A.listArray ((0, 0), (15, 3)) (repeat 0.0)
    , flExplorationRate = 0.9
    }

chooseActionUser :: (MonadIO m) => m FrozenLakeAction
chooseActionUser = (toEnum . read) <$> (liftIO getLine)

chooseActionRandom :: (MonadIO m) => m FrozenLakeAction
chooseActionRandom = toEnum <$> liftIO (Rand.randomRIO (0, 3))

chooseActionQTable :: FrozenLake FrozenLakeAction
chooseActionQTable = do
  fle <- get
  let (exploreRoll, gen') = Rand.randomR (0.0, 1.0) (randomGenerator fle)
  if exploreRoll < flExplorationRate fle
    then do
      let (actionRoll, gen'') = Rand.randomR (0, 3) gen'
      put $ fle { randomGenerator = gen'' }
      return (toEnum actionRoll)
    else do
      let maxIndex = snd $ snd $ maxScore (currentObs fle) (qTable fle)
      put $ fle { randomGenerator = gen' }
      return (toEnum (fromIntegral maxIndex))

learnQTable ::
  FrozenLakeObservation -> FrozenLakeObservation -> Reward -> FrozenLakeAction -> FrozenLake ()
learnQTable obs1 obs2 (Reward reward) action = do
  fle <- get
  let q = qTable fle
      actionIndex = fromIntegral . fromEnum $ action
      prediction = q A.! (obs1, actionIndex)
      target = reward + gamma * (fst $ maxScore obs2 q)
      newValue = prediction + learningRate * (target - prediction)
      newQ = q A.// [((obs1, actionIndex), newValue)]
  put $ fle { qTable = newQ }
  where
    gamma = 0.96
    learningRate = 0.81

maxScore :: FrozenLakeObservation -> A.Array (Word, Word) Double -> (Double, (Word, Word))
maxScore obs table = maximum valuesAndIndices
  where
    indices = (obs, ) <$> [0..3]
    valuesAndIndices = (\i -> (table A.! i, i)) <$> indices
  
{-
playGame :: IO ()
playGame = do
  env <- basicEnv
  void $ execStateT finalAction env
  where
    numEpisodes = 10000
    decayRate = 0.9
    minEpsilon = 0.01

    finalAction :: StateT FrozenLakeEnvironment IO ()
    finalAction = do
      rewards <- forM [1..numEpisodes] $ \i -> do
        resetFrozenLake
        when (i `mod` 100 == 99) $ do
          fle <- get
          let e = flExplorationRate fle
          let newE = max minEpsilon (e * decayRate)
          put $ fle { flExplorationRate = newE }
        (_, reward) <- gameLoop chooseActionQTable
        return reward
      lift $ print (sum rewards)
-}

newtype FrozenLake a = FrozenLake (StateT FrozenLakeEnvironment IO a)
  deriving (Functor, Applicative, Monad)

instance (MonadState FrozenLakeEnvironment) FrozenLake where
  get = FrozenLake get
  put fle = FrozenLake $ put fle

instance MonadIO FrozenLake where
  liftIO act = FrozenLake (liftIO act)

instance EnvironmentMonad FrozenLake where
  type (Observation FrozenLake) = FrozenLakeObservation
  type (Action FrozenLake) = FrozenLakeAction
  type (EnvironmentState FrozenLake) = FrozenLakeEnvironment
  baseEnv = basicEnv
  runEnv env (FrozenLake action) = evalStateT action env
  currentObservation = FrozenLake (currentObs <$> get)
  resetEnv = resetFrozenLake
  stepEnv = stepFrozenLake

instance RenderableEnvironment FrozenLake where
  renderEnv = renderFrozenLake

instance LearningEnvironment FrozenLake where
  learnEnv = learnQTable
  chooseActionBrain = chooseActionQTable
  explorationRate = flExplorationRate <$> get
  reduceExploration decayRate minEpsilon = do
    fle <- get
    let e = flExplorationRate fle
    let newE = max minEpsilon (e * decayRate)
    put $ fle { flExplorationRate = newE }
