--------------------------------------------------------------------
-- CS316 Hangman
--------------------------------------------------------------------
-- Student Names:	Ramsay Meiklem, 	Yordan Stoykov
-- Student Numbers	201343272			201334079
--------------------------------------------------------------------

import Data.List (elem)
--import Data.List

type Secret  = String
type History = String  -- Made guesses
type Guesses = String  -- Future guesses
type Hang    = (Secret, History, Guesses)

data Result = Win | Lose | Alive deriving (Show,Eq)

diff :: Eq a => [a] -> [a] -> [a]
diff [] [] = error "Empty lists"
diff xs ys = [x | x <- xs, not(x `elem` ys)]
--diff xs ys = [x | x <- xs, y <- ys, not (x == y)]

status :: Hang -> Result
status (s, h, g)
					| lives	> 10												= Lose
					| ((length (diff s h)) == 0) 								= Win
					| otherwise 												= Alive
						where lives = length (diff h s)
						
						
iterateUntil :: (a -> a) -> (a -> Bool) -> a -> a
iterateUntil func test x = if test x then x else iterateUntil func test (func x) 

done :: Hang -> Bool
done (s, h, g) = ((not(status (s, h, g) == Alive)) || ((g == [])))
	--where alive = status ("word", "", "w")
	--done (s, h, g) = () || ((length g) == 0))

makeGuess :: Hang -> Hang
makeGuess (s, h, g:gs) = (s, h ++ [g], gs)

hangman  :: Secret -> Guesses -> (Result, Hang)
--hangman = undefined
hangman s g = (status board, board)
		where board = iterateUntil makeGuess done (s, [], g)
