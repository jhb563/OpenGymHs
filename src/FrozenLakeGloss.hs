module FrozenLakeGloss where

import qualified Data.Array as A
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import qualified System.Random as Rand

import FrozenLakeBasic

windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (10, 10)

globalCellSize :: Float
globalCellSize = 50.0

globalXOffset :: Float
globalXOffset = -75.0

globalYOffset :: Float
globalYOffset = -75.0

data GameResult =
  GameInProgress |
  GameWon |
  GameLost
  deriving (Show, Eq)

data World = World
  { environment :: FrozenLakeEnvironment
  , gameResult :: GameResult
  }

main :: IO ()
main = do
  env <- basicEnv
  play windowDisplay white 1 (World env GameInProgress) drawEnvironment handleInputs stepWorld

drawEnvironment :: World -> Picture
drawEnvironment world
  | gameResult world == GameWon = Translate (-150) 0 $ Scale 0.12 0.25
      (Text "You've won! Press enter to restart!")
  | gameResult world == GameLost = Translate (-150) 0 $ Scale 0.12 0.25
      (Text "You've lost :( Press enter to restart.")
  | otherwise = Pictures [tiles, playerMarker]
  where
    observationToCoords :: Word -> (Word, Word)
    observationToCoords w = quotRem w 4

    f :: (Word, TileType) -> Picture
    f (obs, tileType ) =
      let (centerX, centerY) = rowColToCoords . observationToCoords $ obs
          color' = case tileType of
            Goal -> green
            Hole -> black
            _ -> blue
       in Translate centerX centerY (Color color' (Polygon [(-25, -25), (-25, 25), (25, 25), (25, -25)]))

    tiles = Pictures $ map f (A.assocs (grid . environment $ world))

    (px, py) = rowColToCoords . observationToCoords $ (currentObservation . environment $ world)
    playerMarker = translate px py (Color red (ThickCircle 10 3))

rowColToCoords :: (Word, Word) -> (Float, Float)
rowColToCoords (row, col) = (50 * (1.5 - fromIntegral col), 50 * (1.5 - fromIntegral row))

handleInputs :: Event -> World -> World
handleInputs event w
  | gameResult w == GameWon || gameResult w == GameLost = case event of
      (EventKey (SpecialKey KeyEnter) Down _ _) -> World (fle { currentObservation = 0 }) GameInProgress
      _ -> w
  | otherwise = case event of
      (EventKey (SpecialKey KeyUp) Down _ _) -> w {environment = fle { currentObservation = newObservation MoveUp, randomGenerator = finalGen} }
      (EventKey (SpecialKey KeyRight) Down _ _) -> w {environment = fle { currentObservation = newObservation MoveRight, randomGenerator = finalGen} }
      (EventKey (SpecialKey KeyDown) Down _ _) -> w {environment = fle { currentObservation = newObservation MoveDown, randomGenerator = finalGen} }
      (EventKey (SpecialKey KeyLeft) Down _ _) -> w {environment = fle { currentObservation = newObservation MoveLeft, randomGenerator = finalGen} }
  where
    fle = environment w
    currentObs = currentObservation fle
    (slipRoll, gen') = Rand.randomR (0.0, 1.0) (randomGenerator fle)
    allLegalMoves = legalMoves currentObs (dimens fle)
    (randomMoveIndex, finalGen) = Rand.randomR (0, length allLegalMoves - 1) gen'
    newObservation action = if slipRoll >= slipChance fle
      then if action `elem` allLegalMoves
        then applyMoveUnbounded action currentObs (snd . dimens $ fle)
         else currentObs
      else applyMoveUnbounded (allLegalMoves !! randomMoveIndex) currentObs (snd . dimens $ fle)

stepWorld :: Float -> World -> World
stepWorld _ w = case tile of
  Goal -> World fle GameWon
  Hole -> World fle GameLost
  _ -> w
  where
    fle = environment w
    obs = currentObservation fle
    tile = grid fle A.! obs
