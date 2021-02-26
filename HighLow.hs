import System.IO
import System.Random
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe 
{- If the random import gives you issues when compiling, try (on a new terminal):
        stack import random
        stack ghci
   Then continue as normal
-}

-- To run the game do: main

-- Taken from the TwentyOneQuestion assignment solution
-- getLineCorr reads the line and returns the corrected line by removing deleted characters
getLineCorr :: IO [Char]
getLineCorr =
  do 
    line <- getLine
    return (fixdel line)

-- fixdel str removes the character that precedes '\DEL' and '\DEL' itself in str and returns remaining str
fixdel :: [Char] -> [Char]
fixdel str =
  if '\DEL' `elem` str then fixdel (corr str) else str

-- corr (h:t) is the helper function for fixdel
corr :: [Char] -> [Char]
corr ('\DEL':t) = t
corr (h:'\DEL':t) = t
corr (h:t) = h:(corr t)

-- cards
cards = [1..13]

-- deck of cards
decks = cards ++ cards ++ cards ++ cards

-- returns value of card
cardValue :: (Eq a, Num a, Enum a) => a -> a
cardValue n
    | n `elem` [1..13] = n
    | otherwise = 0

toCard n 
    | n == 1 = "Ace (aces are ones!)" -- ++ randomSuit (mkStdGen 100)
    | n == 11 = "Jack" -- ++ randomSuit (mkStdGen 100)
    | n == 12 = "Queen" -- ++ randomSuit (mkStdGen 100)
    | n == 13 = "King" -- ++ randomSuit (mkStdGen 100)
    | otherwise = show n -- ++ randomSuit (mkStdGen 100)

--possibly implement this later
--generator for random numbers
--gen = mkStdGen 3
{-
suits = ["Hearts", "Diamonds", "Spades", "Clubs"]
randomSuit gen = " of " ++ show (suits !! fst (randomR (0, 3) gen))
-}

-- end of game: value (0 = tie, 1 = win, 2 = lose), starting next state
data Result = EndOfGame Int State
    deriving (Eq, Show)

-- a state is a state where currentCard nextCard savedChoice playerfunds bet [remaining cards in deck]
data State = State Int Int String Double Double [Int]
    deriving (Ord, Eq, Show)

-- a highlow game takes a state (current hand), returns result tie, win or lose
type Game = State -> State

highlow :: State -> String -> Result
highlow (State currentCard nextCard savedChoice playerfunds bet currDeck) choice
    | tie currentCard nextCard choice = EndOfGame 0 (State currentCard nextCard choice (playerfunds + bet) 0 currDeck)
    | lose currentCard nextCard choice = EndOfGame 1 (State currentCard nextCard choice playerfunds 0 currDeck)
    | win currentCard nextCard choice = EndOfGame 2 (State currentCard nextCard choice (playerfunds + 1.5*bet) bet currDeck)
    | otherwise = EndOfGame 1 (State currentCard nextCard choice playerfunds 0 currDeck)

-- tie if currentCard and nextCard are equal
tie currentCard nextCard choice = 
    (cardValue currentCard) == (cardValue nextCard)

-- lose if choice is low and currentCard is lower than nextCard
-- lose if choice is high and currentCard is higher than nextCard

lose currentCard nextCard choice
    | choice == "Low" = (cardValue currentCard) < (cardValue nextCard)
    | choice == "High" = (cardValue currentCard) > (cardValue nextCard)
    | otherwise = False

-- win if choice is low and currentCard is higher than nextCard
-- win if choice is high and currentCard is lower than nextCard

win currentCard nextCard choice 
    | choice == "Low" = (cardValue currentCard) > (cardValue nextCard)
    | choice == "High" = (cardValue currentCard) < (cardValue nextCard)
    | otherwise = False

-- draw card to keep track of current card and perhaps nextCard??
drawCard :: State -> IO State
drawCard (State currentCard nextCard savedChoice playerFunds bet []) = drawCard (State currentCard nextCard savedChoice playerFunds bet (createDeck))

drawCard (State currentCard nextCard savedChoice playerFunds bet (f:r)) 
    | nextCard == 0 = drawCard (State currentCard f savedChoice playerFunds bet r)
    | otherwise = return (State nextCard f savedChoice playerFunds bet r)

createDeck = unsafePerformIO (shuffle decks)

-- playersTurn
playersTurn :: State -> IO State
playersTurn p = do
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard p)) ++ "\n"
    putStrLn "High or Low?"
    ans <- getLineCorr
    if ans `elem` ["high", "High"] 
        then do
            nextP <- result (highlow p "High")
            play nextP
    else if ans `elem` ["low", "Low"] 
        then do
            nextP <- result (highlow p "Low")
            play nextP
    else do
            putStrLn "Invalid input. Please try again."
            putStrLn "Enter 'High' or 'Low'"
            playersTurn p

-- gets the users bet
placeBet :: State -> IO State
placeBet (State currentCard nextCard savedChoice playerFunds bet deck) = do
    putStrLn $ "You have $" ++ show playerFunds ++ "\n"
    putStrLn "How much would you like to bet?"
    b <- getLineCorr
    if isDouble b 
        then do
            let playerBet = read b :: Double
            if playerBet <= 0
                    then do 
                    putStrLn "You have to bet more than $0." 
                    placeBet (State currentCard nextCard savedChoice playerFunds 0 deck)
            else if playerBet <= playerFunds 
                then do
                    return (State currentCard nextCard savedChoice (playerFunds - playerBet) playerBet deck) 
                else do
                    putStrLn "You don't have enough money! Please try again." 
                    placeBet (State currentCard nextCard savedChoice playerFunds 0 deck)
        else do
            putStrLn "Invalid input. Please try again."
            placeBet (State currentCard nextCard savedChoice playerFunds 0 deck)

-- determines if a string is a double
-- taken from https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Haskell
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

-- Randomly shuffles deck of cards
-- -- algorithm for shuffle taken from https://wiki.haskell.org/Random_shuffle
-- -- | Randomly shuffle a list
-- -- /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
    
    where
      n = length xs
      newArray :: Int -> [a] -> IO (IOArray Int a)
      newArray n xs =  newListArray (1,n) xs

-- to run program: main
main = do
    play initState

--initial State with $1000 (arbitrary default amount of money)
initState = State 0 0 "" 1000 0 []

play :: State -> IO State
play s = do
    putStrLn "Would you like to play HighLow?"
    ans <- getLineCorr
    if ans `elem` ["y", "yes"] 
        then do 
            a <- drawCard s
            b <- placeBet a
            playersTurn b
    else if ans `elem` ["n","no"]
        then do 
            putStrLn "Thank you for visiting."
            return s
        else do
            putStrLn "Invalid input. Please try again."
            putStrLn "Enter 'y' or 'yes' for YES and 'n' or 'no' for NO."
            play s

-- show player's card
getCurrentCard (State currentCard nextCard savedChoice playerfunds bet currDeck) = currentCard

-- show next card
getNextCard (State currentCard nextCard savedChoice playerfunds bet currDeck) = nextCard

-- show choice
getChoice (State currentCard nextCard savedChoice playerfunds bet currDeck) = savedChoice

-- show bet
getBet (State currentCard nextCard savedChoice playerfunds bet currDeck) = bet 

-- show deck
getDeck (State currentCard nextCard savedChoice playerfunds bet currDeck) = currDeck

-- show playerfunds
getFunds :: State -> Double
getFunds (State currentCard nextCard savedChoice playerfunds bet currDeck) = playerfunds

result :: Result -> IO State
result (EndOfGame 0 s) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard s)) ++ "\n"
    putStrLn $ "Next Card: " ++ show (toCard (getNextCard s)) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------TIE-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ show (getFunds s) ++ "\n"
    return s
result (EndOfGame 1 s) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard s)) ++ "\n"
    putStrLn $ "Next Card: " ++ show (toCard (getNextCard s)) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------LOSE-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ show (getFunds s) ++ "\n"
    return s
result (EndOfGame 2 s) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard s)) ++ "\n"
    putStrLn $ "Next Card: " ++ show (toCard (getNextCard s)) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------WIN-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ show (getFunds s) ++ "\n"
    return s