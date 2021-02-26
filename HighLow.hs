import System.IO
import System.Random
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe 
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
    | win currentCard nextCard choice = EndOfGame 2 (State currentCard nextCard choice (playerfunds + 1.5*bet) 0 currDeck)
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
    p <- placeBet p
   -- playerBet <- toDouble playerBet
    putStrLn $ "Your Card: " ++ show (getCurrentCard p) ++ "\n"
    putStrLn "High or Low?"
    ans <- getLineCorr
    if ans `elem` ["high", "High"] 
        then do
            result (highlow p "High")
            play p
    else if ans `elem` ["low", "Low"] 
        then do
            result(highlow p "Low")
            play p
    else do
            putStrLn "Invalid input. Please try again."
            putStrLn "Enter 'High' or 'Low'"
            playersTurn p



-- gets the users bet
placeBet :: State -> IO State
placeBet (State currentCard nextCard savedChoice playerFunds bet deck) = do
    putStrLn $ "You have $" ++ show (getFunds (State currentCard nextCard savedChoice playerFunds bet deck)) ++ "\n"
    putStrLn "How much would you like to bet?"
    --betPrompt (State currentCard nextCard savedChoice playerFunds bet deck)
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
                    return (State currentCard nextCard savedChoice (playerFunds-playerBet) playerBet deck) 
                else do
                    putStrLn "You don't have enough money! Please try again." 
                    placeBet (State currentCard nextCard savedChoice playerFunds 0 deck)
        else do
            putStrLn "Invalid input. Please try again."
            placeBet (State currentCard nextCard savedChoice playerFunds 0 deck)

{-
--prints bet prompt
--betPrompt :: State -> IO State
betPrompt (State currentCard nextCard savedChoice playerFunds bet deck) = do
    putStrLn $ "You have $" ++ show playerFunds 
    ++ "remaining \nHow much would you like to bet?"  
-}

-- determines if a string is a double
-- taken from https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Haskell
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

{-
--converts a string to a double
toDouble [] = 0.0
toDouble s = do
    read s :: Double 

--converts a double to a string
toString d = do
    show d :: String
-}
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
            playersTurn a
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

-- show playerfunds
getFunds (State currentCard nextCard savedChoice playerfunds bet currDeck) = playerfunds

result :: Result -> IO State
result (EndOfGame 0 s) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (getCurrentCard s) ++ "\n"
    putStrLn $ "Next Card: " ++ show (getNextCard s) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------TIE-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ show (getFunds s) ++ "\n"
    return s
result (EndOfGame 1 s) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (getCurrentCard s) ++ "\n"
    putStrLn $ "Next Card: " ++ show (getNextCard s) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------LOSE-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ show (getFunds s) ++ "\n"
    return s
result (EndOfGame 2 s) = do
    putStrLn ""
    putStrLn $ "Your Card: " ++ show (getCurrentCard s) ++ "\n"
    putStrLn $ "Next Card: " ++ show (getNextCard s) ++ "\n"
    putStrLn $ "Your Choice: " ++ show (getChoice s) ++ "\n"
    putStrLn $ "-------------WIN-------------" ++ "\n"
    putStrLn $ "Player Funds: " ++ show (getFunds s) ++ "\n"
    return s