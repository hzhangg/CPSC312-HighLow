import System.IO

-- cards
cards = [1..13]

-- deck of cards
decks = cards ++ cards ++ cards ++ cards

-- returns value of card
cardValue :: (Eq a, Num a, Enum a) => a -> a
cardValue n
    | n == 1 = 11 
    | n `elem` [2..10] = n
    | n `elem` [11..13] = 10
    | otherwise = 0

-- end of game: value (0 = tie, 1 = win, 2 = lose), starting next state
data Result = EndOfGame Int State
		deriving (Eq, Show)

-- a state is a state where [current card] [next card] choice playerfunds bet [remaining cards in deck]
data State = State [Int] [Int] String Double Double Double [Int] 
         deriving (Ord, Eq, Show)

-- a highlow game takes a state (current hand), returns result tie, win or lose
type Game = State -> Result

highlow :: State -> Result
highlow (State currentCard nextCard choice playerFunds bet currDeck) 
	| tie currentCard nextCard choice = EndOfGame 0 (State currentCard nextCard  choice (playerfunds + bet) 0 currDeck)
	| lose currentCard nextCard choice = EndOfGame 1 (State currentCard nextCard choice playerfunds 0 currDeck)
	| win currentCard nextCard choice = EndOfGame 2 (State currentCard nextCard choice (playerfunds + 1.5*bet) 0 currDeck)


-- draw card to keep track of current card and perhaps nextCard??
drawCard :: State -> IO State



-- playersTurn
playersTurn :: State -> IO State



-- tie if currentCard and nextCard are equal
tie currentCard nextCard choice = 
    if (cardValue currentCard) == (cardValue nextCard)
        then True
        else False

-- lose if choice is low and currentCard is lower than nextCard
-- lose if choice is high and currentCard is higher than nextCard

lose currentCard nextCard choice = 
    if choice == "low"
        then if (cardValue currentCard) < (cardValue nextCard)
            then True
            else False
    if choice == "high"
        then if (cardValue currentCard) > (cardValue nextCard)
            then True
            else False

-- win if choice is low and currentCard is higher than nextCard
-- win if choice is high and currentCard is lower than nextCard

win currentCard nextCard choice = 
    if choice == "low"
        then if (cardValue currentCard) > (cardValue nextCard)
            then True
            else False
    if choice == "high"
        then if (cardValue currentCard) < (cardValue nextCard)
            then True
            else False




-- to run program: play
main = do
    play initState

