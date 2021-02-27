module HighLow where

import System.IO
import System.Random
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe 
import Funds

{- 

1. Load the module
   :load HighLow
2. To run the game do:
   main
   
<< Note>>
If the random import gives you issues when compiling, try (on a new terminal):
    stack import random
    stack ghci

-}


-- =============== --
-- MAIN    SECTION --
-- =============== --

-- a highlow game takes a state (current hand), returns result tie, win or lose
type Game = State -> State

-- end of game: value (0 = tie, 1 = win, 2 = lose), starting next state, prior winnings
data Result = EndOfGame Int State Double
    deriving (Eq, Show)

-- to run program: main
main = do
    printCasino
    play initState


-- [Step #1 ]
-- Let the player select an option in our application
play :: State -> IO State
play s = do
    putStrLn "Would you like to:"
    putStrLn "- Play HighLow? (p/play)"
    putStrLn "- Cash In?      (c/cash)"
    putStrLn "- View Funds?   (v/view)"
    putStrLn "- Quit App?     (q/quit)"
    ans <- getLineCorr
    putStrLn ""
    if (ans `elem` ["p", "play"]) && ((chipsToMoney (getFunds s)) <= 0 )
        then do
            printBroke
            play s
    else if ans `elem` ["p", "play"] 
        then do 
            a <- drawCard s
            b <- placeBet a
            playersTurn b
    else if ans `elem` ["c","cash"]
        then do 
            putStrLn $ "CASHING IN: "
            displayChips(moneyToChips (round (getWinnings s)))
            putStrLn ""
            play (State 0 0 "" (addChips (round (getWinnings s)) (getFunds s)) 0 [] 1 0)
    else if ans `elem` ["v", "view"]
        then do
            putStrLn $ "CURRENT FUNDS: " 
            displayChips (getFunds s)
            putStrLn ""
            play s
    else if ans `elem` ["q","quit"]
        then do 
            printGoodbye
            return s
        else do
            putStrLn "Invalid input. Please try again."
            putStrLn ""
            play s

-- [Step #1.5 ]
-- create new Deck with randomizer algorithm
createDeck :: [Int]
createDeck = unsafePerformIO (shuffle decks)

-- draw card to keep track of current card (and perhaps nextCard)
drawCard :: State -> IO State
drawCard (State currentCard nextCard savedChoice playerFunds bet [] mult winnings) = drawCard (State currentCard nextCard savedChoice playerFunds bet (createDeck) mult winnings)

drawCard (State currentCard nextCard savedChoice playerFunds bet (f:r) mult winnings) 
    | nextCard == 0 = drawCard (State currentCard f savedChoice playerFunds bet r mult winnings)
    | otherwise = return (State nextCard f savedChoice playerFunds bet r mult winnings)


-- [Step #2 ]
-- gets the users bet that they inputted
placeBet :: State -> IO State
placeBet (State currentCard nextCard savedChoice playerFunds bet deck mult winnings) = do
    putStrLn $ "You have $" ++ strRound(show (chipsToMoney playerFunds)) ++ ""
    putStrLn "How much would you like to bet?"
    b <- getLineCorr
    if isDouble b 
        then do
            let playerBet = read b :: Double
            if playerBet <= 0
                    then do 
                    putStrLn "You have to bet more than $0." 
                    placeBet (State currentCard nextCard savedChoice playerFunds 0 deck mult winnings)
            else if playerBet <= fromIntegral(chipsToMoney playerFunds)
                then do
                    putStrLn $ "You now have $" ++ strRound(show (chipsToMoney (subChips (round playerBet) playerFunds))) ++ "\n"
                    return (State currentCard nextCard savedChoice (subChips (round playerBet) playerFunds) playerBet deck mult winnings) 
                else do
                    putStrLn "You don't have enough money! Please try again." 
                    placeBet (State currentCard nextCard savedChoice playerFunds 0 deck mult winnings)
        else do
            putStrLn "Invalid input. Please try again."
            placeBet (State currentCard nextCard savedChoice playerFunds 0 deck mult winnings)

-- [Step #3 ]
-- playersTurn to guees high or low 
playersTurn :: State -> IO State
playersTurn p = do
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard p)) ++ ""
    putStrLn "High or Low?"
    ans <- getLineCorr
    if ans `elem` ["high", "High"] 
        then do
            nextP <- printResult (highlow p "High")
            play nextP
    else if ans `elem` ["low", "Low"] 
        then do
            nextP <- printResult (highlow p "Low")
            play nextP
    else do
            putStrLn "Invalid input. Please try again."
            putStrLn "Enter 'High' or 'Low'"
            playersTurn p



-- =============== --
-- State   SECTION --
-- =============== --

--initial State with $5000 in chips (arbitrary default amount of money)
initState = State 0 0 "" initChips 0 [] 1 0


-- a state is a state where currentCard nextCard savedChoice playerfunds bet [remaining cards in deck] multiplier winnings
data State = State Int Int String [Chips] Double [Int] Double Double
    deriving (Ord, Eq, Show)


-- State getter for player's card
getCurrentCard :: State -> Int
getCurrentCard (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = currentCard

-- State getter for next card
getNextCard :: State -> Int
getNextCard (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = nextCard

-- State getter for choice
getChoice :: State -> String
getChoice (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = savedChoice

-- State getter for playerfunds
getFunds :: State -> [Chips]
getFunds (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = playerfunds

-- State getter for bet
getBet :: State -> Double
getBet (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = bet 

-- State getter for deck
getDeck :: State -> [Int]
getDeck (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = currDeck

-- State getter for multiplier 
getMult :: State -> Double
getMult (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = mult

-- State getter for winnings
getWinnings :: State -> Double
getWinnings (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) = winnings


-- =============== --
-- CARD    SECTION --
-- =============== --

-- cards
cards = [1..13]

-- deck of cards
decks = cards ++ cards ++ cards ++ cards

-- returns value of card
cardValue :: (Eq a, Num a, Enum a) => a -> a
cardValue n
    | n `elem` [1..13] = n
    | otherwise = 0

-- matches the numeric value of a card to its name
toCard :: Int -> String
toCard n 
    | n == 1 = "Ace (aces are ones!)" -- ++ randomSuit (mkStdGen 100)
    | n == 11 = "Jack" -- ++ randomSuit (mkStdGen 100)
    | n == 12 = "Queen" -- ++ randomSuit (mkStdGen 100)
    | n == 13 = "King" -- ++ randomSuit (mkStdGen 100)
    | otherwise = show n -- ++ randomSuit (mkStdGen 100)


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

--possibly implement this later
--generator for random numbers
--gen = mkStdGen 3
{-
suits = ["Hearts", "Diamonds", "Spades", "Clubs"]
randomSuit gen = " of " ++ show (suits !! fst (randomR (0, 3) gen))
-}

-- ================== --
-- HIGHLOW    SECTION --
-- ================== --

-- Given all the player's inputs, play the highlow game and output the result
highlow :: State -> String -> Result
highlow (State currentCard nextCard savedChoice playerfunds bet currDeck mult winnings) choice
    | tie currentCard nextCard choice = 
        let newMult = mult * 1.1
        in EndOfGame 0 (State currentCard nextCard choice playerfunds bet currDeck newMult (newMult * (winnings + bet))) winnings
    | lose currentCard nextCard choice = EndOfGame 1 (State currentCard nextCard choice playerfunds bet currDeck 1 0) winnings
    | win currentCard nextCard choice = 
        let newMult = mult * (cardMult currentCard choice)
        in EndOfGame 2 (State currentCard nextCard choice playerfunds bet currDeck newMult ( newMult * (winnings + bet))) winnings
    | otherwise = EndOfGame 1 (State currentCard nextCard choice playerfunds bet currDeck mult winnings) winnings


-- tie if currentCard and nextCard are equal
tie :: Int -> Int -> String -> Bool
tie currentCard nextCard choice = 
    (cardValue currentCard) == (cardValue nextCard)

-- lose if choice is low and currentCard is lower than nextCard
-- lose if choice is high and currentCard is higher than nextCard
lose :: Int -> Int -> String -> Bool
lose currentCard nextCard choice
    | choice == "Low" = (cardValue currentCard) < (cardValue nextCard)
    | choice == "High" = (cardValue currentCard) > (cardValue nextCard)
    | otherwise = False


-- win if choice is low and currentCard is higher than nextCard
-- win if choice is high and currentCard is lower than nextCard
win :: Int -> Int -> String -> Bool
win currentCard nextCard choice 
    | choice == "Low" = (cardValue currentCard) > (cardValue nextCard)
    | choice == "High" = (cardValue currentCard) < (cardValue nextCard)
    | otherwise = False


-- if the player correctly guessed, give the multiplier based on their guess and the previous card
cardMult :: Int -> String -> Double
cardMult prevCard playerGuess 
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


-- ================= --
-- UTILITY   SECTION --
-- ================= --

-- determines if a string is a double
-- taken from https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Haskell
-- isDouble :: String -> Bool
isDouble s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False


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


-- ================= --
-- TEXT UI   SECTION --
-- ================= --

-- round a (number) string to 2 decimals
strRound :: String -> String
strRound ('.':a:b:c) = '.':[a]++[b]
strRound [] = []
strRound (h:t) = h:(strRound t)


-- prints the corresponding result summary onto console
printResult :: Result -> IO State
printResult (EndOfGame 0 s w) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard s)) ++ "\n"
    putStrLn $ "Next Card: " ++ show (toCard (getNextCard s)) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------TIE-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ strRound(show (chipsToMoney (getFunds s))) ++ "\n"
    putStrLn $ "TIE SUMMARY:"
    putStrLn $ "    = StreakBonus x (PastWinnings + Bet)"
    putStrLn $ "    = " ++ strRound (show (getMult s)) ++ " x ("  ++ strRound(show (w)) ++ " + "  ++ strRound(show (getBet s)) ++ ")"
    putStrLn $ "    = $" ++ strRound (show (getWinnings s)) ++ "\n"
    putStrLn $ "-------------TIE-------------" ++ "\n"
    return s
printResult (EndOfGame 1 s w) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard s)) ++ "\n"
    putStrLn $ "Next Card: " ++ show (toCard (getNextCard s)) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------LOSE-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ strRound(show (chipsToMoney (getFunds s))) ++ "\n"
    putStrLn $ "LOSE SUMMARY:"
    putStrLn $ "- Lost Bet: $" ++ strRound(show (getBet s)) ++ ""
    putStrLn $ "- Lost Winnings: $" ++ strRound(show (w)) ++ "\n"
    putStrLn $ "-------------LOSE-------------" ++ "\n"
    return s
printResult (EndOfGame 2 s w) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (toCard (getCurrentCard s)) ++ "\n"
    putStrLn $ "Next Card: " ++ show (toCard (getNextCard s)) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------WIN-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ strRound(show (chipsToMoney (getFunds s))) ++ "\n"
    putStrLn $ "WIN SUMMARY:"
    putStrLn $ "    = StreakBonus x (PastWinnings + Bet)"
    putStrLn $ "    = " ++ strRound (show (getMult s)) ++ " x ("  ++ strRound(show (w)) ++ " + "  ++ strRound(show (getBet s)) ++ ")"
    putStrLn $ "    = $" ++ strRound (show (getWinnings s)) ++ "\n"
    putStrLn $ "-------------WIN-------------" ++ "\n"
    return s


-- Prints the welcome text at start of application
-- Used an ASCII Art generator
-- Note backslashes need to be doubled as they are escape characters
printCasino :: IO ()
printCasino = do
    putStrLn ""
    putStrLn $ "=============================================================================================="
    putStrLn $ " $$$$$$\\   $$$$$$\\   $$$$$$\\  $$$$$$\\ $$\\   $$\\  $$$$$$\\         $$$$$$\\    $$\\    $$$$$$\\  "
    putStrLn $ "$$  __$$\\ $$  __$$\\ $$  __$$\\ \\_$$  _|$$$\\  $$ |$$  __$$\\       $$ ___$$\\ $$$$ |  $$  __$$\\ "
    putStrLn $ "$$ /  \\__|$$ /  $$ |$$ /  \\__|  $$ |  $$$$\\ $$ |$$ /  $$ |      \\_/   $$ |\\_$$ |  \\__/  $$ |"
    putStrLn $ "$$ |      $$$$$$$$ |\\$$$$$$\\    $$ |  $$ $$\\$$ |$$ |  $$ |        $$$$$ /   $$ |   $$$$$$  |"
    putStrLn $ "$$ |      $$  __$$ | \\____$$\\   $$ |  $$ \\$$$$ |$$ |  $$ |        \\___$$\\   $$ |  $$  ____/" 
    putStrLn $ "$$ |  $$\\ $$ |  $$ |$$\\   $$ |  $$ |  $$ |\\$$$ |$$ |  $$ |      $$\\   $$ |  $$ |  $$ |      "
    putStrLn $ "\\$$$$$$  |$$ |  $$ |\\$$$$$$  |$$$$$$\\ $$ | \\$$ | $$$$$$  |      \\$$$$$$  |$$$$$$\\ $$$$$$$$\\" 
    putStrLn $ " \\______/ \\__|  \\__| \\______/ \\______|\\__|  \\__| \\______/        \\______/ \\______|\\________|"
    putStrLn $ "=============================================================================================="
    putStrLn ""


-- Prints the bankrupt text when trying to bet with insufficient funds
printBroke :: IO ()
printBroke = do 
    putStrLn $ "=================================="
    putStrLn $ "Looks like you're BROKE. GET OUT!!"
    putStrLn $ "================================== \n"


-- Prints the goodbye text during quiting of application
-- Used an ASCII Art generator
-- Note backslashes need to be doubled as they are escape characters
printGoodbye :: IO ()
printGoodbye = do
    putStrLn $ "THANKS FOR VISITING!!"
    putStrLn $ "============================================================================="
    putStrLn $ ".------..------..------..------.     .------..------..------..------..------."
    putStrLn $ "|C.--. ||O.--. ||M.--. ||E.--. |.-.  |A.--. ||G.--. ||A.--. ||I.--. ||N.--. |"
    putStrLn $ "| :/\\: || :/\\: || (\\/) || (\\/) ((5)) | (\\/) || :/\\: || (\\/) || (\\/) || :(): |"
    putStrLn $ "| :\\/: || :\\/: || :\\/: || :\\/: |'-.-.| :\\/: || :\\/: || :\\/: || :\\/: || ()() |"
    putStrLn $ "| '--'C|| '--'O|| '--'M|| '--'E| ((1)) '--'A|| '--'G|| '--'A|| '--'I|| '--'N|"
    putStrLn $ "`------'`------'`------'`------'  '-'`------'`------'`------'`------'`------'"
    putStrLn $ "============================================================================="
    putStrLn ""

