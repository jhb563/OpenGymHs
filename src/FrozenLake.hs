{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module FrozenLake where

import Control.Monad (forM_)
import Control.Monad.State
import qualified Data.Array as A
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import qualified System.Random as Rand

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

type Observation = Word

data Action =
  MoveLeft |
  MoveDown |
  MoveRight |
  MoveUp
  deriving (Show, Eq, Enum)

data FrozenLakeEnvironment = FrozenLakeEnvironment
  { currentObservation :: Observation
  , grid :: A.Array Word TileType
  , slipChance :: Double -- 0 to 1
  , randomGenerator :: Rand.StdGen
  , previousAction :: Maybe Action
  , dimens :: (Word, Word) -- Rows, Cols
  , qTable :: A.Array (Word, Word) Double
  , explorationRate :: Double
  }

resetEnv :: (Monad m) => StateT FrozenLakeEnvironment m Observation
resetEnv = do
  let initialObservation = 0
  fle <- get
  put $ fle { currentObservation = initialObservation, previousAction = Nothing }
  return initialObservation

stepEnv :: (Monad m) => Action -> StateT FrozenLakeEnvironment m (Observation, Double, Bool)
stepEnv act = do
  fle <- get
  let currentObs = currentObservation fle
  let (slipRoll, gen') = Rand.randomR (0.0, 1.0) (randomGenerator fle)
  let allLegalMoves = legalMoves currentObs (dimens fle)
  let (randomMoveIndex, finalGen) = Rand.randomR (0, length allLegalMoves - 1) gen'
  let newObservation = if slipRoll >= slipChance fle
        then if act `elem` allLegalMoves
          then applyMoveUnbounded act currentObs (snd . dimens $ fle)
          else currentObs
        else applyMoveUnbounded (allLegalMoves !! randomMoveIndex) currentObs (snd . dimens $ fle)
  let (done, reward) = case (grid fle) A.! newObservation of
        Goal -> (True, 1.0)
        Hole -> (True, 0.0)
        _ -> (False, 0.0)
  put $ fle {currentObservation = newObservation, randomGenerator = finalGen, previousAction = Just act}
  return (newObservation, reward, done)

-- Does NOT do bounds checking
applyMoveUnbounded :: Action -> Observation -> Word -> Observation
applyMoveUnbounded action currentObs numCols = case action of
  MoveLeft -> currentObs - 1 
  MoveDown -> currentObs + numCols
  MoveRight -> currentObs + 1
  MoveUp -> currentObs - numCols

legalMoves :: Observation -> (Word, Word) -> [Action]
legalMoves observation (numRows, numCols) = catMaybes [left, down, right, up]
  where
    (row, col) = quotRem observation numRows
    left = if col > 0 then Just MoveLeft else Nothing
    down = if row < numRows - 1 then Just MoveDown else Nothing
    right = if col < numCols - 1 then Just MoveRight else Nothing
    up = if row > 0 then Just MoveUp else Nothing

renderEnv :: (MonadIO m) => StateT FrozenLakeEnvironment m ()
renderEnv = do
  fle <- get
  let currentObs = currentObservation fle
  liftIO $ do
    let elements = A.assocs (grid fle)
        numCols = fromIntegral . snd . dimens $ fle
        rows = chunksOf numCols elements
    putStrLn $ case (previousAction fle) of
      Nothing -> ""
      Just a -> "    " ++ show a
    forM_ rows (renderRow currentObs)
  where
    renderRow currentObs row = do
      forM_ row (\(idx, t) -> liftIO $ if idx == currentObs
        then liftIO $ putChar 'X'
        else liftIO $ putChar (tileToChar t))
      putChar '\n'

basicEnv :: IO FrozenLakeEnvironment
basicEnv = do
  gen <- Rand.getStdGen
  return $ FrozenLakeEnvironment
    { currentObservation = 0
    , grid = A.listArray (0, 15) (charToTile <$> "SFFFFHFHFFFHHFFG")
    , slipChance = 0.0
    , randomGenerator = gen
    , previousAction = Nothing
    , dimens = (4, 4)
    , qTable = A.listArray ((0, 0), (15, 3)) (repeat 0.0)
    , explorationRate = 0.9
    }

gameLoop :: (MonadIO m) =>
  StateT FrozenLakeEnvironment m Action ->
  StateT FrozenLakeEnvironment m (Observation, Double)
gameLoop chooseAction = do
  oldObs <- currentObservation <$> get
  newAction <- chooseAction
  (newObs, reward, done) <- stepEnv newAction
  learnQTable oldObs newObs reward newAction
  if done
    then do
      if reward > 0.0 
        then liftIO $ putStrLn "Win"
        else liftIO $ putStrLn "Lose"
      return (newObs, reward)
    else gameLoop chooseAction

chooseActionUser :: (MonadIO m) => m Action
chooseActionUser = (toEnum . read) <$> (liftIO getLine)

chooseActionRandom :: (MonadIO m) => m Action
chooseActionRandom = toEnum <$> liftIO (Rand.randomRIO (0, 3))

chooseActionQTable :: (MonadState FrozenLakeEnvironment m) => m Action
chooseActionQTable = do
  fle <- get
  let (exploreRoll, gen') = Rand.randomR (0.0, 1.0) (randomGenerator fle)
  if exploreRoll < explorationRate fle
    then do
      let (actionRoll, gen'') = Rand.randomR (0, 3) gen'
      put $ fle { randomGenerator = gen'' }
      return (toEnum actionRoll)
    else do
      let maxIndex = snd $ snd $ maxScore (currentObservation fle) (qTable fle)
      put $ fle { randomGenerator = gen' }
      return (toEnum (fromIntegral maxIndex))

learnQTable :: (MonadState FrozenLakeEnvironment m) =>
  Observation -> Observation -> Double -> Action -> m ()
learnQTable obs1 obs2 reward action = do
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

maxScore :: Observation -> A.Array (Word, Word) Double -> (Double, (Word, Word))
maxScore obs table = maximum valuesAndIndices
  where
    indices = (obs, ) <$> [0..3]
    valuesAndIndices = (\i -> (table A.! i, i)) <$> indices

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
        resetEnv
        when (i `mod` 100 == 99) $ do
          fle <- get
          let e = explorationRate fle
          let newE = max minEpsilon (e * decayRate)
          put $ fle { explorationRate = newE }
        (_, reward) <- gameLoop chooseActionQTable
        return reward
      lift $ print (sum rewards)
