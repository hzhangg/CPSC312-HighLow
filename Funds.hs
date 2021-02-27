module Funds where

-- ======================== --
-- PUBLIC FUNCTIONS SECTION --
-- ======================== --

-- [Chips] 
-- represents the player's chips/money
playerChips w r o y g b p m = [(Whites w), (Reds r), (Oranges o), (Yellows y), (Greens g), (Blacks b), (Purples p), (Maroons m)]

emptyChips = playerChips 0 0 0 0 0 0 0 0
oneChips = playerChips 1 1 1 1 1 1 1 1
initChips = playerChips 1000 100 50 25 20 5 1 1

-- Int [Chips] -> [Chips]
-- Add chips to current playerChips array
-- Returns a new playerChips array
addChips pay chips = 
    let newChips = (moneyToChips pay)
    in funcChips (+) newChips chips

-- Int [Chips] -> [Chips]
-- Subtract chips from current playerChips array
-- ~ will pay with current chips if possible
-- ~ otherwise will convert chips to pay
-- Returns a new playerChips array
subChips cost chips = 
    let newChips = (moneyToChips cost)
        budget = chipsToMoney chips
    in 
        if (checkChips (<=) newChips chips) then funcChips (-) chips newChips 
        else moneyToChips (budget - cost)

-- Int [Chips] -> Bool
-- Check if this payment will cost too much
willBankrupt cost chips = 
    let budget = chipsToMoney chips
    in (budget - cost) < 0

-- [Chips] -> Int
-- sums up the playerChips to get total value/budget
chipsToMoney chips = sum (map chipVal chips)

-- Int -> [Chips] 
-- converts a specified amount of money into Chips 
moneyToChips n = 
    let change = mem_toChange n
    in (playerChips
         (length (filter isWhite change))
         (length (filter isRed change))
         (length (filter isOrange change))
         (length (filter isYellow change))
         (length (filter isGreen change))
         (length (filter isBlack change))
         (length (filter isPurple change))
         (length (filter isMaroon change))
        )

-- [Chips] -> consoleUI
-- prints the currently held chips on console
displayChips chips = 
    let 
        (Whites w) = (chips !! 0)
        (Reds r) = (chips !! 1)
        (Oranges o) = (chips !! 2)
        (Yellows y) = (chips !! 3)
        (Greens g) = (chips !! 4)
        (Blacks b) = (chips !! 5)
        (Purples p) = (chips !! 6)
        (Maroons m) = (chips !! 7)
    in do
        putStrLn $ " ==========="
        putStrLn $ "|   CHIPS   |"
        putStrLn $ " ==========="
        putStrLn $ "(Whites)  " ++ show (w) ++ " chips = $" ++ show (white) ++ " x " ++ show (w) ++ " = $" ++ show (w * white) ++" "
        putStrLn $ "(Reds)    " ++ show (r) ++ " chips = $" ++ show (red) ++ " x " ++ show (r) ++ " = $" ++ show (r * red) ++" "
        putStrLn $ "(Oranges) " ++ show (o) ++ " chips = $" ++ show (orange) ++ " x " ++ show (o) ++ " = $" ++ show (o * orange) ++" "
        putStrLn $ "(Yellows) " ++ show (y) ++ " chips = $" ++ show (yellow) ++ " x " ++ show (y) ++ " = $" ++ show (y * yellow) ++" "
        putStrLn $ "(Greens)  " ++ show (g) ++ " chips = $" ++ show (green) ++ " x " ++ show (g) ++ " = $" ++ show (g * green) ++" "
        putStrLn $ "(Blacks)  " ++ show (b) ++ " chips = $" ++ show (black) ++ " x " ++ show (b) ++ " = $" ++ show (b * black) ++" "
        putStrLn $ "(Purples) " ++ show (p) ++ " chips = $" ++ show (purple) ++ " x " ++ show (p) ++ " = $" ++ show (p * purple) ++" "
        putStrLn $ "(Maroons) " ++ show (m) ++ " chips = $" ++ show (maroon) ++ " x " ++ show (m) ++ " = $" ++ show (m * maroon) ++" "
        putStrLn $ "_________________________________________________"
        putStrLn $ "<< Total >> = $" ++ show (chipsToMoney chips) ++ " "


-- ============ --
-- CHIP SECTION --
-- ============ --

data Chips = Whites Int
            | Reds Int
            | Oranges Int
            | Yellows Int
            | Greens Int
            | Blacks Int
            | Purples Int
            | Maroons Int
       deriving (Ord, Eq, Show)

-- chip values --
white = 1
red = 5
orange = 10
yellow = 20
green = 25
black = 100
purple = 500
maroon = 1000

chipVal (Whites n) = n * white
chipVal (Reds n) = n * red
chipVal (Oranges n) = n * orange
chipVal (Yellows n) = n * yellow
chipVal (Greens n) = n * green
chipVal (Blacks n) = n * black
chipVal (Purples n) = n * purple
chipVal (Maroons n) = n * maroon

isWhite n = n == white
isRed n = n == red
isOrange n = n == orange
isYellow n = n == yellow
isGreen n = n == green
isBlack n = n == black
isPurple n = n == purple
isMaroon n = n == maroon

-- (Int -> Int -> Int) -> [Chips] -> [Chips] -> [Chips]
-- abstracted chip modification function 
funcChips f 
    [(Whites w), (Reds r), (Oranges o), (Yellows y), (Greens g), (Blacks b), (Purples p), (Maroons m)] 
    [(Whites w2), (Reds r2), (Oranges o2), (Yellows y2), (Greens g2), (Blacks b2), (Purples p2), (Maroons m2)] = 
        (playerChips
            (f w w2)
            (f r r2)
            (f o o2)
            (f y y2)
            (f g g2)
            (f b b2)
            (f p p2)
            (f m m2)
        )

-- (Int -> Int -> Bool) -> [Chips] -> [Chips] -> Bool
-- checks if enough chips  
checkChips f 
    [(Whites w), (Reds r), (Oranges o), (Yellows y), (Greens g), (Blacks b), (Purples p), (Maroons m)] 
    [(Whites w2), (Reds r2), (Oranges o2), (Yellows y2), (Greens g2), (Blacks b2), (Purples p2), (Maroons m2)] = 
        and [
            (f w w2),
            (f r r2),
            (f o o2),
            (f y y2),
            (f g g2),
            (f b b2),
            (f p p2),
            (f m m2)
        ]

-- ========================= --
-- CHANGE CONVERSION SECTION --
-- ========================= --

-- [[Int]]
-- The memoization data structure (an array)
-- Used the top answer's for guidence https://stackoverflow.com/questions/3208258/memoization-in-haskell
mem_list = map (toChange mem_toChange) [0..]
-- Int -> [Int]
-- The getter function
mem_toChange n = mem_list !! n


-- (Int -> [Int]) -> Int -> [Int]
-- a breadth-first search utilizing memoization
-- will minimize number of coins required
-- returns a list of coins used as change for n
-- Ex. 40 -> [20, 20] and not [25,10,5]
toChange mf n =
    if n < white then []
    else if n < red then bestof [
                            white:(mf (n - white))
                            ]
    else if n < orange then bestof [
                            white:(mf (n - white)),
                            red:(mf (n - red))
                            ]
    else if n < yellow then bestof [
                            white:(mf (n - white)),
                            red:(mf (n - red)),
                            orange:(mf (n - orange))
                            ]
    else if n < green then bestof [
                            white:(mf (n - white)),
                            red:(mf (n - red)),
                            orange:(mf (n - orange)),
                            yellow:(mf (n - yellow))
                            ]
    else if n < black then bestof [
                            white:(mf (n - white)),
                            red:(mf (n - red)),
                            orange:(mf (n - orange)),
                            yellow:(mf (n - yellow)),
                            green:(mf (n - green))
                            ]
    else if n < purple then bestof [
                            white:(mf (n - white)),
                            red:(mf (n - red)),
                            orange:(mf (n - orange)),
                            yellow:(mf (n - yellow)),
                            green:(mf (n - green)),
                            black:(mf (n - black))
                            ]
    else if n < maroon then bestof [
                            white:(mf (n - white)),
                            red:(mf (n - red)),
                            orange:(mf (n - orange)),
                            yellow:(mf (n - yellow)),
                            green:(mf (n - green)),
                            black:(mf (n - black)),
                            purple:(mf (n - purple))
                            ]
    else bestof [
                            white:(mf (n - white)),
                            red:(mf (n - red)),
                            orange:(mf (n - orange)),
                            yellow:(mf (n - yellow)),
                            green:(mf (n - green)),
                            black:(mf (n - black)),
                            purple:(mf (n - purple)),
                            maroon:(mf (n - maroon))
                            ]

-- [[Int]] -> [Int]
-- Given a list of paths, returns the shortest path 
bestof [] = []
bestof [h] = h
bestof ([]:t) = bestof t
bestof (h:t) = shortest h (bestof t)

-- [Int] -> [Int] -> [Int]
-- Given two lists, returns the shorter of the two lists
shortest x ans
    | (length x) < (length ans) = x
    | (length x) > (length ans) = ans
    | otherwise = x