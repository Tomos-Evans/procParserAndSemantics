# Proc Parser and Natural Semantics

A Parser and Natural Semantics for the basic imperitive programming language, `Proc`. Proc is a basic imperative programming language that is proposed and defined in "Semantics with Applications, by H.R. Nielson and F. Nielson". This project will take you through building a parser using MegaParsec, and later into defineing both dynamic and mixed natural semantics for the Proc language. Both the Parser and the Natural Semantics are written in the functional programming language Haskell.

## Setting up

If you would like to run this parser and semeantics yourself there are a number of packages that are required. I will be installing them globally with [cabal](https://www.haskell.org/cabal/),  but feel free to use a sandbox if you feel there may be confilcts.

The parseing libary `megaparsec` is required as it provides basic low level parsers such as `string` etc. `hspec` and `hspec-megaparsec` are not strictly required, but I would deffintely recomend them as they are used to manage the automated testing of the project. If you do not plan on extending or tinkering with the source, then you can leave the tests as they are.

To install the packages enter the following commands in the terminal:
``` sh
cabal install megaparsec
cabal install hspec
cabal install hspec-megaparsec
```
Note that if you would like to use a `sandbox` you must issue the command:
``` sh 
cabal sandbox init
```
 in the directory that contains your project.
 
 
 
 ## Static Evaluation
 
You may notice that the static evaluation of the Proc Language currently fails some of its tests! I know what the problem is, just waiting for the time to finnish off some functions. The Static elvaluation is logically seperate from the Dynamic and Mixed evaluations, so those two should be fine!
 
 
