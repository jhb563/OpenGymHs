{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Environment
import FrozenLakeTensor

main :: IO ()
main = do
  (env :: FrozenLakeEnvironment) <- basicEnv
  results <- runEnv env action
  putStrLn "Done!"
  where
    -- action = resetEnv >> (gameRenderLoop chooseActionUser :: FrozenLake (FrozenLakeObservation, Reward))
    action = (resetEnv >> gameLearningIterations :: FrozenLake [Reward])
