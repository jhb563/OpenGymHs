{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Environment
import BlackjackEnvironment

main :: IO ()
main = do
  (env :: BlackjackEnvironment) <- basicEnv
  _ <- runEnv env action
  putStrLn "Done!"
  where
    action = (resetEnv >> gameRenderLoop chooseActionUser :: Blackjack (BlackjackObservation, Reward))
