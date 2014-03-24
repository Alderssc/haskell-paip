-- anotherGrammar.hs corresponds to the straightforward approach of
-- section 2.1. This program suffers from the same problem as the one presented; namely that implementing new rules requires knowledge of programming in Haskell
-- whereas, ideally, the creation of new rules should rely only on linguistic 
-- conventions.
import System.Random (randomRIO)

randChoice :: [a] -> IO a 
randChoice xs = randomRIO (0, length(xs) - 1) >>= \n -> return (xs !! n)

concatenate :: [String] -> String
concatenate xs = foldr (++) "" xs

sentence :: IO String
sentence =
  nounPhrase >>= \np ->
  verbPhrase >>= \vp ->
  return $ concatenate [np, " ", vp]
  
nounPhrase :: IO String
nounPhrase = 
  noun >>= \n ->
  article >>= \a ->
  return $ concatenate [a, " ", n]

article :: IO String
article = randChoice ["a", "an", "the"]

noun :: IO String
noun = randChoice ["woman", "man", "book", "tree"]

verb :: IO String
verb = randChoice ["hugs", "lifts", "drops", "throws"]

verbPhrase :: IO String
verbPhrase = 
  verb >>= \v ->
  nounPhrase >>= \np ->
  return $ concatenate [v, " ", np]

main :: IO ()
main = sentence >>= putStrLn
