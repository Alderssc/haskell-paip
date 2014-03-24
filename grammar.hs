-- grammar.hs is my attempt at making something like the program
-- presented in 2.3 (pg. 39) 
-- the idea is that the rules are specified through data types
-- and simple functions generate productions according to the rules
import System.Random (randomRIO)
type Verb = String
type Noun = String
type Article = String

data NounPhrase = NP Article Noun 
data VerbPhrase = VP Verb NounPhrase 
data Sentence = Sentence NounPhrase VerbPhrase 

verbs :: [Verb]
verbs = ["hugs", "drops"]

nouns :: [Noun]
nouns = ["woman", "man"]

articles :: [Article]
articles = ["a", "the"]

chooseR :: [a] -> IO a
chooseR xs = randomRIO (0, length xs - 1) >>= \n -> return (xs !! n)

genNP :: IO NounPhrase
genNP = chooseR articles >>= \a -> 
          chooseR nouns >>= \n ->
            return $ NP a n

genVP :: IO VerbPhrase
genVP = chooseR verbs >>= \v ->
          genNP >>= \np ->
            return $ VP v np

genSentence :: IO Sentence
genSentence = genNP >>= \np ->
                genVP >>= \vp ->
                  return $ Sentence np vp

instance Show NounPhrase where
  show (NP a n) = a ++ " " ++ n 
instance Show VerbPhrase where
  show (VP v np) = v ++ " " ++ show np
instance Show Sentence where
  show (Sentence np vp) = show np ++ " " ++ show vp

main :: IO ()
main = genSentence >>= print
