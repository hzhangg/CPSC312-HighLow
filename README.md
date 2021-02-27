# CPSC312-HighLow
This repository is for our HighLow project for CPSC312
Wiki Link: https://wiki.ubc.ca/Course:CPSC312-2021/HighLow

To try our app, run the "main" function found in HighLow.hs
1. Load the module
   :load HighLow
2. To run the game do:
   main
   
<< Note>>
If the random import gives you issues when compiling, try (on a new terminal):
    stack import random
    stack ghci


<< Description >>
HighLow is a card game where the Player attempts to guess if the next card drawn is "Higher" or "Lower" than the previous.
Our variation has the following features:


**Dynamic Payout System**
The Payout dynamically changes proportional to the odds of a guess being correct.
- This is given in the form of a multiplier to your payout
For instance, 
	Guessing "High" when given an Ace(#1) has ~90% to be correct -> Low  Multipler (x1.2)
	Guessing "Low " when given an Ace(#1) has ~10% to be correct -> High Multipler (x3.0)


**Win-Streak System**
With each consequtive win, you are given a multiplier to your Payout.
- This multiplier stacks with previous multipliers gained.
- Losing will reset the multipler to 1.0 and discard all stacked multipliers

For instance,
	Assume we always draw an Ace(#1) and guess "High", so multipler is (x1.2)
		Win #1: Multiplier Gained = x1.2, Total Multipler = (1.0)x(1.2)             = 1.20
		Win #2: Multiplier Gained = x1.2, Total Multipler = (1.0)x(1.2)x(1.2)       = 1.44
		Win #2: Multiplier Gained = x1.2, Total Multipler = (1.0)x(1.2)x(1.2)x(1.2) = 1.73	


**Compounded Betting System**
The player may bet any non-negative amount of money that they possess.

Upon Victory
- Your bet will be translated into your winnings 
- You may continue your streak or cash in your current winnings 

Upon Losing 
- Your bet is lost and all winnings you had are discarded
- Should you run out of money, you must leave the casino

Upon Continuing Streak
- At this point, you may also bet even more money to contribute to you winnings
- Specifically, your winnings are calculated based on your Multiplier, PayoutSoFar, and Bet 
--> Multiplier x (PayoutSoFar + Bet)
- The implications is that your payout will follow a compounded pattern 

For instance,
	Assume in each win session, Multiplier gained is 1.2, Bet was $100
		Win #1: 1.20 x (0 + $100)    = $120.00
		Win #2: 1.44 x ($120 + $100) = $316.80
		Win #3: 1.73 x ($317 + $100) = $721.41

** Chip System **
We built a representation of the Player's Funds and Payout as Money Chips as in real casinos 

Sample Values:
- White  = $1
- Red    = $5
- Orange = $10
- Yellow = $20
- Green  = $25
- Black  = $100
- Purple = $500
- Maroon = $1000

We implemented the BFS algorithm along with memoization to compute the Chips dealed
- This function minimizes the amount of Chips needed to satisfy the amount

For instance,
	$40 with a greedy algorithm (translate highest cost chips first)
		Has the chips [25, 10 ,5] = 3 Chips
	$40 with our algorithm minimizes the chips needed 
		Has the chips [20, 20]    = 2 Chips


** Card System **
We implemented a representation of the 13 unique cards in a deck
- We used a randomization algorithm to create and shuffle the deck 
- The drawn card is random

Also:
- Upon winning, drawn cards remain removed from deck 
- Upon losing, drawn cards remain removed from deck 
- If cashing in the payout, the deck is reset 
- If the deck runs out, the deck is reset

**Input Cleaning and Simple TextUI**
We implemented some input processing.
We played with text formatting and printing for decorative and readability purposes







