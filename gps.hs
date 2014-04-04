-- a first-pass naive implementation of newell & simon's general problem
-- solver
type State = String
type World = [State]
type Action = (State, State)
type Knowledge = [Action]

perform :: Action -> World -> World
perform (s0,s1) w = 
	if s0 `isIn` w
		then addS s1 (delS s0 w)
		else w

addS :: State -> World -> World
addS s w = w ++ [s]

delS :: State -> World -> World
delS s w = filter (\x -> x /= s) w

isIn :: State -> World -> Bool
isIn s w = length (filter (\x -> x == s) w) /= 0

findSuitableAction :: Knowledge -> World -> [Action]
findSuitableAction [] _ = []
findSuitableAction _ [] = []
findSuitableAction ((x,y):as) w = 
	if x `isIn` w
		then [(x,y)] ++ findSuitableAction as w
		else findSuitableAction as w

reachableW :: Knowledge -> World -> [World]
reachableW k w = (map (\a -> (perform a w)) (findSuitableAction k w))

solve k w0 w1 = 
	if w0 == w1
		then True
		else or (map (\w -> solve k w w1) (reachableW k w0))


