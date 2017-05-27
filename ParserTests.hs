module ParserTests where

import ProcParser
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import Test.Hspec
import Test.Hspec.Megaparsec

main :: IO ()
main = hspec $ do
    numSpec
    identifierSpec
    aexpSpec
    bexpSpec
    decvSpec
    decpSpec
    stmSpec

--------------------------------------------------------------------------------
-- Numerals
--      digit ::= '0' | .. | '9'
--      num   ::= digit+
--------------------------------------------------------------------------------

numSpec :: Spec
numSpec = do
    describe "num" $ do
        it "returns the parsed number" $ do
            parse num "" "451073" `shouldParse` 451073

        it "stops parsing at a non-digit" $ do
            parse num "" "126abc" `shouldParse` 126

        it "does not parse negative numbers" $ do
            parse num "" `shouldFailOn` "-12722"

--------------------------------------------------------------------------------
-- Identifier
--      identifier ::= (lower | upper) (lower | upper | digit)*
--------------------------------------------------------------------------------

identifierSpec :: Spec
identifierSpec = do
    describe "identifier" $ do
        it "returns the identifier" $ do
            parse identifier "" "anIdentifier" `shouldParse` "anIdentifier"

        it "parses if the first character is uppercase" $ do
            parse identifier "" "Identifier" `shouldParse` "Identifier"

        it "parses if a number is contained in the identifier" $ do
            parse identifier "" "id1234" `shouldParse` "id1234"

        it "fails if the first character is a digit" $ do
            parse identifier "" `shouldFailOn` "1Identifier"

        it "fails if the identifier is a keyword" $ do
            parse identifier "" `shouldFailOn` "true"

        it "fails if it the identifier contains symbols" $ do
            parse identifier "" "a'" `shouldParse` "a"
            parse identifier "" "x_a" `shouldParse` "x"


--------------------------------------------------------------------------------
-- Arithmetic Expressions:
--      aexp ::= num
--             | var
--             | aexp '+' aexp
--             | aexp '*' aexp
--             | aexp '-' aexp
--------------------------------------------------------------------------------

aexpSpec :: Spec
aexpSpec = do
    describe "aexp" $ do
        it "parses numbers" $ do
            parse aexp "" "36543" `shouldParse` (N 36543)

        it "parses variables" $ do
            parse aexp "" "myVar" `shouldParse` (V "myVar")

        it "parses multiplication" $ do
            parse aexp "" "34 * x" `shouldParse` (Mult (N 34) (V "x"))

        it "multiplication is left associative" $ do
            parse aexp "" "1 * 2 * 3" `shouldParse` (Mult (Mult (N 1) (N 2)) (N 3))

        it "parses addition" $ do
            parse aexp "" "21 + 5" `shouldParse ` (Add (N 21) (N 5))

        it "addition is left associative" $ do
            parse aexp "" "1 + 2 + 3" `shouldParse` (Add (Add (N 1) (N 2)) (N 3))

        it "parses subtraction" $ do
            parse aexp "" "6 - y" `shouldParse` (Sub (N 6) (V "y"))

        it "subtraction is left associative" $ do
            parse aexp "" "1 - 2 - 3" `shouldParse` (Sub (Sub (N 1) (N 2)) (N 3))

        it "multiplication has precedence over addition" $ do
            parse aexp "" "3 + 6 * x" `shouldParse` (Add (N 3) (Mult (N 6) (V "x")))

        it "multiplication has precedence over subtraction" $ do
            parse aexp "" "3 - 6 * x" `shouldParse` (Sub (N 3) (Mult (N 6) (V "x")))

        it "addition and subtraction have the same precedence" $ do
            parse aexp "" "6 + x - 3" `shouldParse` (Sub (Add (N 6) (V "x")) (N 3))
            parse aexp "" "6 - x + 3" `shouldParse` (Add (Sub (N 6) (V "x")) (N 3))

        it "gives precedence to bracketed expressions" $ do
            parse aexp "" "6 * (x + 3)" `shouldParse` (Mult (N 6) (Add (V "x") (N 3)))

        it "allows brackets at the top level" $ do
            parse aexp "" "(x + 5)" `shouldParse` (Add (V "x") (N 5))
--------------------------------------------------------------------------------
-- Boolean Expressions:
--      bexp ::= 'true'
--             | 'false'
--             | aexp '=' aexp
--             | aexp '<=' aexp
--             | '!' bexp
--             | bexp '&' bexp
--------------------------------------------------------------------------------


bexpSpec :: Spec
bexpSpec = do
    describe "bexp" $ do
        it "parses true" $ do
            parse bexp "" "true" `shouldParse` TRUE

        it "parses false" $ do
            parse bexp "" "false" `shouldParse` FALSE

        it "parses negation" $ do
            parse bexp "" "!false" `shouldParse` (Neg FALSE)

        it "parses and" $ do
            parse bexp "" "true & false" `shouldParse` (And TRUE FALSE)

        it "and is left associative" $ do
            parse bexp "" "true & false & false" `shouldParse` (And (And TRUE FALSE) FALSE)

        it "parses less than or equal to" $ do
            parse bexp "" "5 <= 7" `shouldParse` (Le (N 5) (N 7))

        it "le should not be allowed to be chained" $ do
            parse bexp "" "x <= y <= z" `shouldParse` (Le (V "x") (V "y"))

        it "allows braces at the second level when using le" $ do
            parse bexp "" "(5) <= (5)" `shouldParse` (Le (N 5) (N 5))

        it "parses equal" $ do
            parse bexp "" "x = y" `shouldParse` (Eq (V "x") (V "y"))

        it "equals should not be allowed to be chained" $ do
            parse bexp "" "x = y = z" `shouldParse` (Eq (V "x") (V "y"))

        it "allows braces at the second level when using equals" $ do
            parse bexp "" "(5) = (5)" `shouldParse` (Eq (N 5) (N 5))

        it "gives precedence to bracketed expressions" $ do
            parse bexp "" "true & (false & false)" `shouldParse` (And TRUE (And FALSE FALSE))

        it "allows brackets at the top level" $ do
            parse bexp "" "(true)" `shouldParse` TRUE

--------------------------------------------------------------------------------
-- Variable Declarations:
--      Dv ::= 'var' var ':=' aexp ';' Dv | ε
--------------------------------------------------------------------------------

decvSpec :: Spec
decvSpec = do
    describe "decV" $ do
        it "parses no variable declarations" $ do
            parse decV "" " " `shouldParse` []

        it "parses one variable declaration" $ do
            parse decV "" "var x := 5 + y;" `shouldParse` [("x", Add (N 5) (V "y"))]

        it "parses multiple variable declarations" $ do
            parse decV "" "var x := 5 + y; var y := 5;" `shouldParse` [("x", Add (N 5) (V "y")), ("y", (N 5))]

        it "parses with brackets around arithmetic expressions" $ do
            parse decV "" "var x := (5 + y); var y := (5);" `shouldParse` [("x", Add (N 5) (V "y")), ("y", (N 5))]

        it "fails if the keyword var is missing" $ do
            parse decV "" "x := 5 + y;" `shouldParse` []

        it "fails if the symbol := is missing" $ do
            parse decV "" `shouldFailOn` "var x 5 + y;"

        it "fails if a semicolon is missing" $ do
            parse decV "" `shouldFailOn` "var x := 5 + y"

        it "fails if an arithmetic expression is missing" $ do
            parse decV "" `shouldFailOn` "var x := ;"
--------------------------------------------------------------------------------
-- Procedure Declarations:
--      Dp ::= 'proc' proc 'is' Stm ';' Dp | ε
--------------------------------------------------------------------------------

decpSpec :: Spec
decpSpec = do
    describe "decP" $ do
        it "parses no proc declarations" $ do
            parse decP "" " " `shouldParse` []

        it "parses one proc declaration" $ do
            parse decP "" "proc fac is skip;" `shouldParse` [("fac", Skip)]

        it "parses multiple proc declarations" $ do
            parse decP "" "proc x is skip; proc y is x:=5;" `shouldParse` [("x", Skip), ("y", Ass "x" (N 5))]

        it "parses when composition and brackets are used" $ do
            parse decP "" "proc x is (skip;skip);" `shouldParse` [("x", Comp Skip Skip)]

        it "fails if the keyword proc is missing" $ do
            parse decP "" "fac is skip;" `shouldParse` []

        it "fails if the keyword is is missing" $ do
            parse decP "" `shouldFailOn` "proc fac skip;"

        it "fails if the semicolon is missing" $ do
            parse decP "" `shouldFailOn` "proc fac is skip"
--------------------------------------------------------------------------------
-- Statements:
--      Stm ::= 'skip'
--            | var ':=' aexp
--            | Stm ';' Stm
--            | 'if' bexp 'then' Stm 'else' Stm
--            | 'while' bexp 'do' Stm
--            | 'begin' Dv Dp Stm 'end'
--            | 'call' proc
--------------------------------------------------------------------------------

stmSpec :: Spec
stmSpec = do
    describe "stm" $ do
        it "parses skip" $ do
            parse stm "" "skip" `shouldParse` Skip

        it "parses assignment" $ do
            parse stm "" "x := 5" `shouldParse` (Ass "x" (N 5))

        it "parses an if statement" $ do
            parse stm "" "if (true) then skip else x := 5" `shouldParse` (If TRUE Skip (Ass "x" (N 5)))

        it "fails if an if statement is missing the keyword then" $ do
            parse stm "" `shouldFailOn `"if (true) skip else skip"

        it "fails if an if statement is missing the keyword else" $ do
            parse stm "" `shouldFailOn `"if (true) then skip"

        it "parses a while statement" $ do
            parse stm "" "while (false) do skip" `shouldParse` (While FALSE Skip)

        it "fails if a while is not matched with the keyword do" $ do
            parse stm "" `shouldFailOn` "while false skip"

        it "parses a block with no declarations" $ do
            parse stm "" "begin skip end" `shouldParse` (Block [] [] Skip)

        it "parses a block with variable declarations" $ do
            parse stm "" "begin var x := 5; var y := 6; skip end" `shouldParse` (Block [("x", N 5), ("y", N 6)] [] Skip)

        it "parses a block with proc declarations" $ do
            parse stm "" "begin proc fac1 is skip; proc fac2 is skip; skip end" `shouldParse` (Block [] [("fac1", Skip), ("fac2", Skip)] Skip)

        it "parses with variable and block declarations" $ do
            parse stm "" "begin var x := 5; proc fac1 is skip; skip end" `shouldParse` (Block [("x", N 5)] [("fac1", Skip)] Skip)

        it "parses a call" $ do
            parse stm "" "call fac" `shouldParse` (Call "fac")

        it "parses composition" $ do
            parse stm "" "skip;skip" `shouldParse` (Comp Skip Skip)

        it "composition is right associative" $ do
            parse stm "" "x:=1;x:=2;x:=3" `shouldParse` (Comp (Ass "x" (N 1)) (Comp (Ass "x" (N 2)) (Ass "x" (N 3))))

        it "parses with brackets around statements" $ do
            parse stm "" "(skip;skip)" `shouldParse` (Comp Skip Skip)

        it "brackets group statements 1" $ do
            parse stm "" "(while true do x:=1);skip" `shouldParse` (Comp (While TRUE (Ass "x" (N 1))) Skip)

        it "brackets group statements 2" $ do
            parse stm "" "while true do (x:=1;skip)" `shouldParse` (While TRUE (Comp ((Ass "x" (N 1))) Skip))

        it "ignores a whole line comment at the start" $ do
            parse stm "" "//This is a comment\nskip" `shouldParse` Skip

        it "ignores a whole line comment" $ do
            parse stm "" "skip;\n//Comment\nskip" `shouldParse` (Comp Skip Skip)

        it "ignores an inline comment at the start" $ do
            parse stm "" "/*inline*/skip" `shouldParse` Skip

        it "ignores an inline comment" $ do
            parse stm "" "skip;/*inline*/skip" `shouldParse` (Comp Skip Skip)

        it "parses fac_loop" $ do
            parse stm "" "/*fac_loop (p.23)*/\ny:=1;\nwhile !(x=1) do (\n y:=y*x;\n x:=x-1\n)" `shouldParse` (Comp (Ass "y" (N 1)) (While (Neg (Eq (V "x") (N 1))) (Comp (Ass "y" (Mult (V "y") (V "x"))) (Ass "x" (Sub (V "x") (N 1))))))

        it "parses fac_call" $ do
            parse stm "" "//fac call (p.55)\nbegin\nproc fac is\nbegin\nvar z:=x;\nif x=1 then\nskip\nelse\nx:=x-1;\ncall fac;\ny:=z*y\nend;\ny:=1;\ncall fac\nend" `shouldParse` (Block [] [("fac",Block [("z",V "x")] [] (If (Eq (V "x") (N 1)) Skip (Comp (Ass "x" (Sub (V "x") (N 1))) (Comp (Call "fac") (Ass "y" (Mult (V "z") (V "y")))))))] (Comp (Ass "y" (N 1)) (Call "fac")))

        it "parses scope_test" $ do
            parse stm "" "//scope test (p.53)\nbegin\nvar x:=0;\nproc p is x:=x*2;\nproc q is call p;\nbegin\nvar x:=5;\nproc p is x:=x+1;\ncall q\nend\nend" `shouldParse` (Block [("x", (N 0))] [("p", Ass "x" (Mult (V "x") (N 2))), ("q", Call "p")] (Block [("x", N 5)] [("p", (Ass "x" (Add (V "x") (N 1))))] (Call "q")))
