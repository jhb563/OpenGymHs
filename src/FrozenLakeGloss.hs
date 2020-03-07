module FrozenLakeGloss where

import qualified Data.Array as A
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import qualified System.Random as Rand

import FrozenLakeBasic

windowDisplay :: Display
windowDisplay = InWindow "Window" (400, 400) (10, 10)

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
  play windowDisplay white 20 (World env GameInProgress) drawEnvironment handleInputs stepWorld

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

    renderTile :: (Word, TileType) -> Picture
    renderTile (obs, tileType ) =
      let (centerX, centerY) = rowColToCoords . observationToCoords $ obs
          color' = case tileType of
            Goal -> green
            Hole -> black
            _ -> blue
       in Translate centerX centerY (Color color' (Polygon [(-50, -50), (-50, 50), (50, 50), (50, -50)]))

    tiles = Pictures $ map renderTile (A.assocs (grid . environment $ world))

    (px, py) = rowColToCoords . observationToCoords $ (currentObservation . environment $ world)
    playerMarker = translate px py (Color red (ThickCircle 10 3))

rowColToCoords :: (Word, Word) -> (Float, Float)
rowColToCoords (row, col) = (100 * (fromIntegral col - 1.5), 100 * (1.5 - fromIntegral row))

handleInputs :: Event -> World -> World
handleInputs event w
  | gameResult w == GameWon || gameResult w == GameLost = case event of
      (EventKey (SpecialKey KeyEnter) Down _ _) -> World (resetEnv' fle) GameInProgress
      _ -> w
  | otherwise = case event of
      (EventKey (SpecialKey KeyUp) Down _ _) -> w {environment = finalEnv MoveUp }
      (EventKey (SpecialKey KeyRight) Down _ _) -> w {environment = finalEnv MoveRight }
      (EventKey (SpecialKey KeyDown) Down _ _) -> w {environment = finalEnv MoveDown }
      (EventKey (SpecialKey KeyLeft) Down _ _) -> w {environment = finalEnv MoveLeft }
      _ -> w
  where
    fle = environment w
    finalEnv action =
      let (fe, _, _) = stepEnv' action fle
      in  fe

stepWorld :: Float -> World -> World
stepWorld _ w = case tile of
  Goal -> World fle GameWon
  Hole -> World fle GameLost
  _ -> w
  where
    fle = environment w
    obs = currentObservation fle
    tile = grid fle A.! obs
