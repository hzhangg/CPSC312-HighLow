import System.IO

-- cards
cards = [1..13]

-- deck of cards
decks :: (Eq t, Num t) => t -> [Int]
decks 0 = []
decks n = cards ++ cards ++ cards ++ cards ++ decks (n-1)

-- returns name of card
cardName :: (Eq a, Num a) => a -> [Char]
cardName 1 = "ACE"
cardName 2 = "TWO"
cardName 3 = "THREE"
cardName 4 = "FOUR"
cardName 5 = "FIVE"
cardName 6 = "SIX"
cardName 7 = "SEVEN"
cardName 8 = "EIGHT"
cardName 9 = "NINE"
cardName 10 = "TEN"
cardName 11 = "JACK"
cardName 12 = "QUEEN"
cardName 13 = "KING"


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

-- tie if currentCard and nextCard are equal

-- lose if choice is low and currentCard is lower than nextCard
-- lose if choice is high and currentCard is higher than nextCard

-- win if choice is low and currentCard is higher than nextCard
-- lose if choice is high and currentCard is lower than nextCard

-- to run program: play
main = do
    play initState

