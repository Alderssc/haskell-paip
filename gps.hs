-- a first-pass naive implementation of newell & simon's general problem
-- solver
type State = String
type World = [State]
type Knowledge = [Action]
type Goal = [State]

data Action = A (String, Precondition, Effect) 
data Precondition = Pc [State] deriving (Show)
data Effect = E [State] deriving (Show)

instance Show Action where
		show (A (s, Pc x, E y)) = s

check :: Precondition -> World -> Bool
check (Pc st) w = and (map (\x -> x `isIn` w) st)

perform :: Action -> World -> World
perform (A (name, pres, efx)) w = 
	if (check pres w)
		then update efx (consume pres w) 
		else w

consume :: Precondition -> World -> World
consume (Pc []) w = w
consume (Pc (p:ps)) w = consume (Pc ps) (delS p w)

update :: Effect -> World -> World
update (E []) w = w
update (E (e:es)) w = update (E es) (addS e w)

addS :: State -> World -> World
addS s w = w ++ [s]

delS :: State -> World -> World
delS s w = filter (\x -> x /= s) w

isIn :: State -> World -> Bool
isIn s w = length (filter (\x -> x == s) w) /= 0

findSuitableAction :: Knowledge -> World -> [Action]
findSuitableAction [] _ = []
findSuitableAction _ [] = []
findSuitableAction (A (name,pres,efx):as) w = 
	if (check pres w)
		then [A (name, pres,efx)] ++ findSuitableAction as w
		else findSuitableAction as w

expand :: Knowledge -> World -> [World]
expand k w = (map (\a -> (perform a w)) (findSuitableAction k w))

goalReached :: Goal -> World -> Bool
goalReached goals world = and [and (map (\goal -> goal `isIn` world) goals), (length goals == length world)]

solvable :: Knowledge -> World -> Goal -> Bool
solvable k w g 
	| goalReached g w == True = True
	| (length (findSuitableAction k w) == 0) = False
  | otherwise = or (map (\x -> solvable k x g) (expand k w))

bestMove :: [Action] -> Knowledge -> World -> Goal -> Action
bestMove [] k w g = A ("There is no best action!", Pc [], E [])
bestMove (a:as) k w g = 
	if isWinner a k w g
		then a
		else bestMove as k w g

getName :: Action -> String
getName (A (s, Pc x, E y)) = s

solve :: Knowledge -> World -> Goal -> [Action]
solve k w g
  | goalReached g w = []
	| getName (bestMove (findSuitableAction k w) k w g) == "There is no best action!" = []
	| otherwise = [bestMove (findSuitableAction k w) k w g] ++ (solve k (perform (bestMove (findSuitableAction k w) k w g) w) g)

isWinner :: Action -> Knowledge -> World -> Goal -> Bool
isWinner a k w g = solvable k (perform a w) g
