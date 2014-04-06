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
instance Eq Action where
		(==) (A (sa, Pc xa, E ya)) (A (sb, Pc xb, E yb)) = and [(sa == sb), (xa == xb), (ya == yb)]

check :: Precondition -> World -> Bool
check (Pc st) w = and (map (\x -> x `isIn` w) st)

perform :: Action -> World -> World
perform (A (name, pres, efx)) w
	| check pres w = update efx (consume pres w)
	| otherwise = w

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

getPreconds :: Action -> Precondition
getPreconds (A (name, Pc ps, E efx)) = Pc ps

rest :: [a] -> [a]
rest (a:as) = as

findSuitableAction :: Knowledge -> World -> [Action]
findSuitableAction actions world
		| or [actions == [], world == []] = []
		| check (getPreconds (actions !! 0)) world = [actions !! 0] ++ findSuitableAction (rest actions) world
		| otherwise = findSuitableAction (rest actions) world

expand :: Knowledge -> World -> [World]
expand k w = (map (\a -> (perform a w)) (findSuitableAction k w))

goalReached :: Goal -> World -> Bool
goalReached goals world = and [and (map (\goal -> goal `isIn` world) goals), (length goals == length world)]

solvable :: Knowledge -> World -> Goal -> Bool
solvable k w g 
	| goalReached g w = True
	| (findSuitableAction k w) == [] = False
  | otherwise = or (map (\x -> solvable k x g) (expand k w))

bestMove :: [Action] -> Knowledge -> World -> Goal -> Action
bestMove [] k w g = A ("There is no best action!", Pc [], E [])
bestMove (a:as) k w g
	| isWinner a k w g = a
	| otherwise = bestMove as k w g

solve :: Knowledge -> World -> Goal -> [Action]
solve k w g
  | goalReached g w = []
	| (bestMove (findSuitableAction k w) k w g) == A ("There is no best action!", Pc [], E[]) = []
	| otherwise = [bestMove (findSuitableAction k w) k w g] ++ (solve k (perform (bestMove (findSuitableAction k w) k w g) w) g)

isWinner :: Action -> Knowledge -> World -> Goal -> Bool
isWinner a k w g = solvable k (perform a w) g

winLottery = A ("Win the lottery", Pc ["poor"], E ["rich"])
getDiscovered = A ("Get discovered by a talent scout", Pc ["unknown"], E ["famous"])
knowledge = [winLottery, getDiscovered]
goal = ["famous", "rich"]
world = ["poor", "unknown"]
