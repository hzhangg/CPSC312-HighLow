import System.IO
import System.Random
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe 
import Funds
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

-- end of game: value (0 = tie, 1 = win, 2 = lose), starting next state, prior winnings
data Result = EndOfGame Int State Double
    deriving (Eq, Show)

-- a state is a state where currentCard nextCard savedChoice playerfunds bet [remaining cards in deck] multiplier winnings
data State = State Int Int String Double Double [Int] Double Double
    deriving (Ord, Eq, Show)

-- a highlow game takes a state (current hand), returns result tie, win or lose
type Game = State -> State

highlow :: State -> String -> Result
highlow (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) choice
    | tie currentCard nextCard choice = 
        let newMult = mult * 1.1
        in EndOfGame 0 (State currentCard nextCard choice playerfunds bet currDeck newMult (newMult * (winnings + bet))) winnings
    | lose currentCard nextCard choice = EndOfGame 1 (State currentCard nextCard choice playerfunds bet currDeck 1 0) winnings
    | win currentCard nextCard choice = 
        let newMult = (mult * (m currentCard choice) )
        in EndOfGame 2 (State currentCard nextCard choice playerfunds bet currDeck newMult ( newMult * (winnings + bet))) winnings
    | otherwise = EndOfGame 1 (State currentCard nextCard choice (playerfunds + winnings) bet currDeck mult winnings) winnings

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

-- if the player correctly guessed, give the multiplier based on their guess and the previous card
m prevCard playerGuess 
    | prevCard == 1 && playerGuess == "High" || prevCard == 13 && playerGuess == "Low" 
        = 1.2
    | prevCard == 2 && playerGuess == "High" || prevCard == 12 && playerGuess == "Low" 
        = 1.3
    | prevCard == 3 && playerGuess == "High" || prevCard == 11 && playerGuess == "Low" 
        = 1.35
    | prevCard == 4 && playerGuess == "High" || prevCard == 10 && playerGuess == "Low" 
        = 1.4
    | prevCard == 5 && playerGuess == "High" || prevCard == 9 && playerGuess == "Low" 
        = 1.425
    | prevCard == 6 && playerGuess == "High" || prevCard == 8 && playerGuess == "Low" 
        = 1.45
    | prevCard == 7 && playerGuess == "High" || prevCard == 7 && playerGuess == "Low" 
        = 1.5
    | prevCard == 8 && playerGuess == "High" || prevCard == 6 && playerGuess == "Low" 
        = 1.625
    | prevCard == 9 && playerGuess == "High" || prevCard == 5 && playerGuess == "Low" 
        = 1.75
    | prevCard == 10 && playerGuess == "High" || prevCard == 4 && playerGuess == "Low" 
        = 2.0
    | prevCard == 11 && playerGuess == "High" || prevCard == 3 && playerGuess == "Low" 
        = 2.25
    | prevCard == 12 && playerGuess == "High" || prevCard == 2 && playerGuess == "Low" 
        = 2.5
    | prevCard == 13 && playerGuess == "High" || prevCard == 1 && playerGuess == "Low" 
        = 3.0
    | otherwise = 1.5
    
    


-- draw card to keep track of current card and perhaps nextCard??
drawCard :: State -> IO State
drawCard (State currentCard nextCard savedChoice playerFunds bet [] mult winnings) = drawCard (State currentCard nextCard savedChoice playerFunds bet (createDeck) mult winnings)

drawCard (State currentCard nextCard savedChoice playerFunds bet (f:r) mult winnings) 
    | nextCard == 0 = drawCard (State currentCard f savedChoice playerFunds bet r mult winnings)
    | otherwise = return (State nextCard f savedChoice playerFunds bet r mult winnings)

createDeck = unsafePerformIO (shuffle decks)

-- playersTurn
playersTurn :: State -> IO State
playersTurn p = do
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard p)) ++ ""
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
placeBet (State currentCard nextCard savedChoice playerFunds bet deck mult winnings) = do
    putStrLn $ "You have $" ++ strRound(show playerFunds) ++ ""
    putStrLn "How much would you like to bet?"
    b <- getLineCorr
    if isDouble b 
        then do
            let playerBet = read b :: Double
            if playerBet <= 0
                    then do 
                    putStrLn "You have to bet more than $0." 
                    placeBet (State currentCard nextCard savedChoice playerFunds 0 deck mult winnings)
            else if playerBet <= playerFunds 
                then do
                    putStrLn $ "You now have $" ++ strRound(show (playerFunds - playerBet)) ++ "\n"
                    return (State currentCard nextCard savedChoice (playerFunds - playerBet) playerBet deck mult winnings) 
                else do
                    putStrLn "You don't have enough money! Please try again." 
                    placeBet (State currentCard nextCard savedChoice playerFunds 0 deck mult winnings)
        else do
            putStrLn "Invalid input. Please try again."
            placeBet (State currentCard nextCard savedChoice playerFunds 0 deck mult winnings)

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
initState = State 0 0 "" 1000 0 [] 1 0

play :: State -> IO State
play s = do
    putStrLn "Would you like to:"
    putStrLn "- Play HighLow? (p/play)"
    putStrLn "- Cash In?      (c/cash)"
    putStrLn "- View Funds?   (v/view)"
    putStrLn "- Quit App?     (q/quit)"
    ans <- getLineCorr
    putStrLn ""
    if ans `elem` ["p", "play"] 
        then do 
            a <- drawCard s
            b <- placeBet a
            playersTurn b
    else if ans `elem` ["c","cash"]
        then do 
            putStrLn "Use Display method"
            putStrLn ""
            play (State 0 0 "" ((getFunds s) + (getWinnings s)) 0 [] 1 0)
    else if ans `elem` ["v", "view"]
        then do
            putStrLn "use display method"
            putStrLn ""
            play s
    else if ans `elem` ["q","quit"]
        then do 
            putStrLn $ "Thank you for visiting. \n"
            return s
        else do
            putStrLn "Invalid input. Please try again."
            putStrLn ""
            play s

-- show player's card
getCurrentCard (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = currentCard

-- show next card
getNextCard (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = nextCard

-- show choice
getChoice (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = savedChoice

-- show bet
getBet (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = bet 

-- show deck
getDeck (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = currDeck

-- show multiplier 
getMult (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = mult

-- show winnings
getWinnings (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = winnings

-- show playerfunds
getFunds :: State -> Double
getFunds (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = playerfunds

-- round string to 2 decimals
strRound ('.':a:b:c) = '.':[a]++[b]
strRound [] = []
strRound (h:t) = h:(strRound t)

result :: Result -> IO State
result (EndOfGame 0 s w) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard s)) ++ "\n"
    putStrLn $ "Next Card: " ++ show (toCard (getNextCard s)) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------TIE-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ strRound(show (getFunds s)) ++ "\n"
    putStrLn $ "Winnings Summary:"
    putStrLn $ "    = Multiplier x (PastWinnings + Bet)"
    putStrLn $ "    = " ++ strRound (show (getMult s)) ++ " x ("  ++ strRound(show (w)) ++ " + "  ++ strRound(show (getBet s)) ++ ")"
    putStrLn $ "    = $" ++ strRound (show (getWinnings s)) ++ "\n"
    return s
result (EndOfGame 1 s w) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard s)) ++ "\n"
    putStrLn $ "Next Card: " ++ show (toCard (getNextCard s)) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------LOSE-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ strRound(show (getFunds s)) ++ ""
    putStrLn $ "You LOST:"
    putStrLn $ "- Your Bet: $" ++ strRound(show (getBet s)) ++ ""
    putStrLn $ "- Your Winnings: $" ++ strRound(show (w)) ++ "\n"
    return s
result (EndOfGame 2 s w) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard s)) ++ "\n"
    putStrLn $ "Next Card: " ++ show (toCard (getNextCard s)) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------WIN-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ strRound(show (getFunds s)) ++ "\n"
    putStrLn $ "Winnings Summary:"
    putStrLn $ "    = Multiplier x (PastWinnings + Bet)"
    putStrLn $ "    = " ++ strRound (show (getMult s)) ++ " x ("  ++ strRound(show (w)) ++ " + "  ++ strRound(show (getBet s)) ++ ")"
    putStrLn $ "    = $" ++ strRound (show (getWinnings s)) ++ "\n"
    return s