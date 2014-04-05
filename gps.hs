-- a first-pass naive implementation of newell & simon's general problem
-- solver
type State = String
type World = [State]
type Action = (Precondition,Effect)
type Knowledge = [Action]

data Precondition = Pc [State]
data Effect = E [State]

check :: Precondition -> World -> Bool
check (Pc st) w = and (map (\x -> x `isIn` w) st)

perform :: Action -> World -> World
perform (pres, efx) w = 
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
findSuitableAction ((pres,efx):as) w = 
	if (check pres w)
		then [(pres,efx)] ++ findSuitableAction as w
		else findSuitableAction as w

reachableW :: Knowledge -> World -> [World]
reachableW k w = (map (\a -> (perform a w)) (findSuitableAction k w))

solve k w0 w1 = 
	if w0 == w1
		then True
		else or (map (\w -> solve k w w1) (reachableW k w0))


