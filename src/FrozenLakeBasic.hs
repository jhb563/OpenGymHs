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
    , slipChance = 0.33
    , randomGenerator = gen
    , previousAction = Nothing
    , dimens = (4, 4)
    }

gameLoop :: (MonadIO m) =>
  StateT FrozenLakeEnvironment m Action ->
  StateT FrozenLakeEnvironment m (Observation, Double)
gameLoop chooseAction = do
  renderEnv 
  newAction <- chooseAction
  (newObs, reward, done) <- stepEnv newAction
  if done
    then do
      liftIO $ print reward
      liftIO $ putStrLn "Episode Finished"
      renderEnv
      return (newObs, reward)
    else gameLoop chooseAction

chooseActionUser :: (MonadIO m) => m Action
chooseActionUser = (toEnum . read) <$> (liftIO getLine)

chooseActionRandom :: (MonadIO m) => m Action
chooseActionRandom = toEnum <$> liftIO (Rand.randomRIO (0, 3))

playGame :: IO ()
playGame = do
  env <- basicEnv
  void $ execStateT (gameLoop chooseActionUser) env
