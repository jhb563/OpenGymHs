{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Blackjack where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import qualified Data.Array as A
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
  , qTable :: A.Array (Word, Word, Word, Word) Double
  , explorationRate :: Double
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
    (qTable bje)
    (explorationRate bje)
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

chooseActionUser :: (MonadIO m) => m BlackjackAction
chooseActionUser = (toEnum . read) <$> (liftIO getLine)

chooseActionRandom :: (MonadIO m) => m BlackjackAction
chooseActionRandom = toEnum <$> liftIO (Rand.randomRIO (0, 1))

chooseActionQTable :: (MonadState BlackjackEnvironment m) => m BlackjackAction
chooseActionQTable = do
  bje <- get
  let (exploreRoll, gen') = Rand.randomR (0.0, 1.0) (randomGenerator bje)
  if exploreRoll < explorationRate bje
    then do
      let (actionRoll, gen'') = Rand.randomR (0, 1) gen'
      put $ bje { randomGenerator = gen''}
      return (toEnum actionRoll)
    else do
      let (_, (_, _, _, maxIndex)) = maxScore (currentObservation bje) (qTable bje)
      put $ bje { randomGenerator = gen' }
      return (toEnum (fromIntegral maxIndex))

learnQTable :: (MonadState BlackjackEnvironment m) =>
  BlackjackObservation ->
  BlackjackObservation ->
  Double ->
  BlackjackAction ->
  m ()
learnQTable obs1 obs2 reward action = do
  bje <- get
  let q = qTable bje
      qIndex = makeQIndex obs1 action
      prediction = q A.! qIndex
      target = reward + gamma * (fst $ maxScore obs2 q)
      newValue = prediction + learningRate * (target - prediction)
      newQ = q A.// [(qIndex, newValue)]
  put $ bje { qTable = newQ }
  where
    gamma = 0.96
    learningRate = 0.81

maxScore :: BlackjackObservation -> A.Array (Word, Word, Word, Word) Double -> (Double, (Word, Word, Word, Word))
maxScore obs table = maximum valuesAndIndices
  where
    indices = (makeQIndex obs) <$> [Hit, Stand]
    valuesAndIndices = (\i -> (table A.! i, i)) <$> indices

makeQIndex :: BlackjackObservation -> BlackjackAction -> (Word, Word, Word, Word)
makeQIndex (BlackjackObservation playerScore hasAce dealerCard) action =
  ( playerScore
  , if hasAce then 1 else 0
  , fromIntegral . fromEnum $ dealerCard
  , fromIntegral . fromEnum $ action
  )

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
    (A.listArray ((0,0,0,0), (30, 1, 12, 1)) (repeat 0.0))
    1.0

playGame :: IO ()
playGame = do
  env <- basicEnv
  env' <- execStateT resetEnv env
  void $ execStateT finalAction env'
  where
    numEpisodes = 10000
    decayRate = 1.0
    minEpsilon = 0.01

    finalAction :: StateT BlackjackEnvironment IO ()
    finalAction = do
      rewards <- forM [1..numEpisodes] $ \i -> do
        resetEnv
        when (i `mod` 100 == 99) $ do
          bje <- get
          let e = explorationRate bje
          let newE = max minEpsilon (e * decayRate)
          put $ bje { explorationRate = newE }
        (_, reward) <- gameLoop chooseActionQTable
        return reward
      lift $ print (sum rewards)
