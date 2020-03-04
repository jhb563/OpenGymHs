module Blackjack where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Data.List (sort, intercalate)
import qualified System.Random as Rand
import System.Random.Shuffle (shuffleM)

data Card =
  Two | Three | Four | Five |
  Six | Seven | Eight | Nine |
  Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum)

baseDeck :: [Card]
baseDeck = concat $ replicate 4 fullSuit
  where
    fullSuit = [ Two, Three, Four, Five
               , Six, Seven, Eight, Nine
               , Ten, Jack, Queen, King, Ace
               ]

shuffledDeck :: Rand.StdGen -> ([Card], Rand.StdGen)
shuffledDeck gen = runRand (shuffleM baseDeck) gen

cardScore :: Card -> Word
cardScore Two = 2
cardScore Three = 3
cardScore Four = 4
cardScore Five = 5
cardScore Six = 6
cardScore Seven = 7
cardScore Eight = 8
cardScore Nine = 9
cardScore Ten = 10
cardScore Jack = 10
cardScore Queen = 10
cardScore King = 10
cardScore Ace = 1

cardToString :: Card -> String
cardToString Two = "2"
cardToString Three = "3"
cardToString Four = "4"
cardToString Five = "5"
cardToString Six = "6"
cardToString Seven = "7"
cardToString Eight = "8"
cardToString Nine = "9"
cardToString Ten = "10"
cardToString Jack = "J"
cardToString Queen = "Q"
cardToString King = "K"
cardToString Ace = "A"

-- Returns the base sum, as well as a boolean if we have
-- a "usable" Ace.
baseScore :: [Card] -> (Word, Bool)
baseScore cards = (score, score <= 11 && Ace `elem` cards)
  where
    score = sum (cardScore <$> cards)

scoreHand :: [Card] -> Word
scoreHand cards = if hasUsableAce then score + 10 else score
  where
    (score, hasUsableAce) = baseScore cards

isNaturalBlackjack :: [Card] -> Bool
isNaturalBlackjack cards = length cards == 2 && sort scores == [1, 10]
  where
    scores = cardScore <$> cards

data BlackjackAction = Hit | Stand
  deriving (Show, Eq, Enum)

data BlackjackObservation = BlackjackObservation
  { playerScore :: Word
  , playerHasAce :: Bool
  , dealerCardShowing :: Card
  } deriving (Show)

data BlackjackEnvironment = BlackjackEnvironment
  { currentObservation :: BlackjackObservation
  , dealerHand :: (Card, Card, [Card]) -- Shown card, hidden card, dealt cards
  , playerHand :: [Card]
  , deck :: [Card]
  , randomGenerator :: Rand.StdGen
  , playerHasStood :: Bool
  } deriving (Show)

resetEnv :: (Monad m) => StateT BlackjackEnvironment m BlackjackObservation
resetEnv = do
  bje <- get
  let (newDeck, newGen) = shuffledDeck (randomGenerator bje)
  let ([playerCard1, playerCard2, dealerHiddenCard, dealerShowCard], playDeck) = splitAt 4 newDeck
  let playerHand = [playerCard1, playerCard2]
  let initialObservation = BlackjackObservation (scoreHand playerHand) (Ace `elem` playerHand) dealerShowCard
  put $ BlackjackEnvironment 
    initialObservation
    (dealerShowCard, dealerHiddenCard, [])
    playerHand
    playDeck
    newGen
    False
  return initialObservation

renderEnv :: (MonadIO m) => StateT BlackjackEnvironment m ()
renderEnv = do
  bje <- get
  let pHand = playerHand bje
  let (dealerShow, dealerHidden, dealerExtraCards) = dealerHand bje
  liftIO $ do
    if (playerHasStood bje)
      then do
        let finalDealerHand = dealerShow : dealerHidden : dealerExtraCards
        putStrLn (handToString finalDealerHand)
        print (scoreHand finalDealerHand)
      else do
        putStr (handToString [dealerShow])
        putStrLn " X"
    putStrLn ""
    putStrLn (handToString pHand)
    print (scoreHand pHand)
  where
    handToString hand = intercalate " " (cardToString <$> hand)

stepEnv :: (Monad m) => BlackjackAction -> StateT BlackjackEnvironment m (BlackjackObservation, Double, Bool)
stepEnv action = do
  bje <- get
  case action of
    Hit -> do
      let (topCard : remainingDeck) = deck bje -- Assume we never get to end of the deck
          pHand = playerHand bje
          currentObs = currentObservation bje
          newPlayerHand = topCard : pHand
          newScore = scoreHand newPlayerHand
          newObservation = currentObs { playerScore = newScore, playerHasAce = playerHasAce currentObs || topCard == Ace}
      put $ bje { currentObservation = newObservation, playerHand = newPlayerHand, deck = remainingDeck }
      if newScore > 21
        then return (newObservation, 0.0, True)
        else if newScore == 21
          then playOutDealerHand
          else return (newObservation, 0.0, False)
    Stand -> do
      put $ bje { playerHasStood = True }
      playOutDealerHand

playOutDealerHand :: (Monad m) => StateT BlackjackEnvironment m (BlackjackObservation, Double, Bool)
playOutDealerHand = do
  bje <- get
  let (showCard, hiddenCard, restCards) = dealerHand bje
      currentDealerScore = scoreHand (showCard : hiddenCard : restCards)
  if currentDealerScore < 17
    then do
      let (topCard : remainingDeck) = deck bje
      put $ bje { dealerHand = (showCard, hiddenCard, topCard : restCards), deck = remainingDeck}
      playOutDealerHand
    else do
      let playerScore = scoreHand (playerHand bje)
          currentObs = currentObservation bje
      if playerScore > currentDealerScore || currentDealerScore > 21
        then return (currentObs, 1.0, True)
        else if playerScore == currentDealerScore
          then return (currentObs, 0.5, True)
          else return (currentObs, 0.0, True)

gameLoop :: (MonadIO m) =>
  StateT BlackjackEnvironment m BlackjackAction ->
  StateT BlackjackEnvironment m (BlackjackObservation, Double)
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

chooseActionUser :: (MonadIO m) => m BlackjackAction
chooseActionUser = (toEnum . read) <$> (liftIO getLine)

chooseActionRandom :: (MonadIO m) => m BlackjackAction
chooseActionRandom = toEnum <$> liftIO (Rand.randomRIO (0, 1))

-- Call reset to make it properly. The only important thing here is the random generator
basicEnv :: IO BlackjackEnvironment
basicEnv = do
  gen <- Rand.getStdGen
  let (d, newGen) = shuffledDeck gen
  return $ BlackjackEnvironment
    (BlackjackObservation 0 False Ace)
    (Ace, Ace, [])
    []
    []
    gen
    False

playGame :: IO ()
playGame = do
  env <- basicEnv
  env' <- execStateT resetEnv env
  void $ execStateT (gameLoop chooseActionUser) env'
