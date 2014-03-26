import System.Random (randomRIO)
type Symbol = String 
data Rule = NonTerminal Symbol [Rule] | Terminal Symbol [Symbol] deriving (Show)

sentence :: Rule
sentence = NonTerminal "Sentence" [np, vp]

np :: Rule
np = NonTerminal "NounPhrase" [a, n]

a :: Rule
a = Terminal "Article" ["the", "a"]

n :: Rule
n = Terminal "Noun" ["woman", "flower", "bee"]

v :: Rule
v = Terminal "Verb" ["plants", "pollinates", "annoys"]

vp :: Rule
vp = NonTerminal "VerbPhrase" [v, np]

rewrite :: Rule -> IO Symbol
rewrite (Terminal name syms) = chooseR syms
rewrite (NonTerminal name []) = return ""
rewrite (NonTerminal name (r:rs)) = (rewrite r) >>= \a -> (rewrite (NonTerminal name rs)) >>= \b -> return $ Main.concat a b

concat :: Symbol -> Symbol -> Symbol
concat x y | x == ""  = y
           | y == ""  = x
           | otherwise = x ++ " " ++ y

chooseR :: [a] -> IO a
chooseR xs = randomRIO (0, length xs - 1) >>= \n -> return (xs !! n)

main :: IO ()
main = rewrite sentence >>= putStrLn
