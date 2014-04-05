Haskell-PAIP
============
This repository contains code I've written while reading Norvig's Paradigms of Artificial Intelligence Programming. I'd been wanting to learn Haskell, so it seemed like a fun exercise to implement the programs in that language, as opposed to Scheme.

Programs for Chapter 2
----------------------
* anotherGrammars.hs - This was the first one I made, it was meant to be simple and closely following the program in 2.1.

* grammar.hs - The second grammar program I wrote. Mainly, I was learning about specifying my own data types here.

* ruleGrammar.hs - My favorite. This one was inspired by the rule based solution in 2.3.

Programs for Chapter 4
----------------------
* gps.hs - my first attempt at implementing the GPS. Here is an example session, where the secret to riches and success is revealed!

> *Main> let winLottery = A ("Win the lottery", Pc ["poor"], E ["rich"])
> *Main> let getDiscovered = A ("Get discovered by talent scout", Pc ["unknown"], E ["famous"])
> *Main> let initialW = ["poor", "unknown"]
> *Main> let knowledge = [winLottery, getDiscovered]
> *Main> let goal = ["rich", "famous"]
> *Main> solve knowledge initialW goal
> [Win the lottery,Get discovered by talent scout]
