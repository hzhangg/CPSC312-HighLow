import System.IO


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

-- a state is a state where currentCard nextCard choice playerfunds bet [remaining cards in deck]
data State = State Int Int String Double Double Double [Int] 
    deriving (Ord, Eq, Show)

-- a highlow game takes a state (current hand), returns result tie, win or lose
type Game = State -> Result

highlow :: State -> Result
highlow (State currentCard nextCard choice playerFunds bet currDeck) 
    | tie currentCard nextCard choice = EndOfGame 0 (State currentCard nextCard  choice (playerfunds + bet) 0 currDeck)
    | lose currentCard nextCard choice = EndOfGame 1 (State currentCard nextCard choice playerfunds 0 currDeck)
    | win currentCard nextCard choice = EndOfGame 2 (State currentCard nextCard choice (playerfunds + 1.5*bet) 0 currDeck)
    | otherwise = ContinueGame (State currentCard nextCard choice playerfunds 0 currDeck)

-- tie if currentCard and nextCard are equal
tie currentCard nextCard choice = 
    if (cardValue currentCard) == (cardValue nextCard)
        then True
        else False

-- lose if choice is low and currentCard is lower than nextCard
-- lose if choice is high and currentCard is higher than nextCard

lose currentCard nextCard choice
    | choice == "low" = if (cardValue currentCard) < (cardValue nextCard)
        then True
        else False
    | choice == "high" = if (cardValue currentCard) > (cardValue nextCard)
        then True
        else False

-- win if choice is low and currentCard is higher than nextCard
-- win if choice is high and currentCard is lower than nextCard

win currentCard nextCard choice 
    | choice == "low" = if (cardValue currentCard) > (cardValue nextCard)
        then True
        else False
    | choice == "high" = if (cardValue currentCard) < (cardValue nextCard)
        then True
        else False

-- draw card to keep track of current card and perhaps nextCard??
drawCard :: State -> IO State
drawCard (State currentCard nextCard choice playerFunds bet []) = drawCard (State currentCard nextCard choice playerFunds bet (createDeck))

drawCard (State currentCard nextCard choice playerFunds bet (f:r)) 
    | nextCard == 0 = drawCard (State currentCard f choice playerFunds bet r)
    | otherwise = return (State nextCard f choice playerFunds bet r)


-- playersTurn
playersTurn :: State -> IO State
playersTurn p = do
    putStrLn "High or Low?"
    ans <- getLineCorr
    putStrLn "Got here"
    newState <- highlow 
    playersTurn




-- to run program: play
main = do
    play (State 0 0 "" 0 0 0 [])

play :: State -> IO State
play s = do
    putStrLn "Would you like to play HighLow?"
    ans <- getLineCorr
    if ans `elem` ["y", "yes"] 
        then do 
                a <- drawCard (State 0 0 "" 0 0 0 [])
                playersTurn a
    else if ans `elem` ["n","no"]
        then do 
            putStrLn "Thank you for visiting."
            return (State 0 0 "" 0 0 0 [])
        else do
            putStrLn "Invalid input. Please try again."
            putStrLn "Enter 'y' or 'yes' for YES and 'n' or 'no' for NO."
            play s