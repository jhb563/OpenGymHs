{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Environment
import FrozenLakeEnvironment

main :: IO ()
main = do
  (env :: FrozenLakeEnvironment) <- basicEnv
  _ <- runEnv env action
  putStrLn "Done!"
  where
    -- action = resetEnv >> (gameRenderLoop chooseActionUser :: FrozenLake (FrozenLakeObservation, Reward))
    action = (resetEnv >> gameLearningIterations :: FrozenLake [Reward])
