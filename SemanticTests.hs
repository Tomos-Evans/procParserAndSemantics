-- The tests for the Semantics of Proc


module SemanticsTests where

import Test.Hspec
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import ProcParser
import ProcSemantics

main :: IO ()
main = hspec $ do
    aValSpec
    bValSpec
    updateSpec
    updateManySpec
    restoreSpec
    sDynamicSpec
    sMixedSpec
    sStaticSpec

-- A state in which "x" evaulates to 1, "y" evaulates to 2 and "z"
-- evaulates to 3. All other variables evaulate to 0.
testState :: State
testState "x" = 1
testState "y" = 2
testState "z" = 3
testState _   = 0

emptyState :: State
emptyState var = 0

-- A program used to test that the statement evaulators correctly with different
-- scope rules.
scopeStm :: Stm
scopeStm = Block [("x",N 0)] [("p",Ass "x" (Mult (V "x") (N 2))),("q",Call "p")] (Block [("x",N 5)] [("p",Ass "x" (Add (V "x") (N 1)))] (Comp (Call "q") (Ass "y" (V "x"))))

-- A recursive factorial program, that populates the variable "y" with "x"!.
recursiveFac :: Stm
recursiveFac = (Block [] [("fac",Block [("z",V "x")] [] (If (Eq (V "x") (N 1)) Skip (Comp (Comp (Ass "x" (Sub (V "x") (N 1))) (Call "fac")) (Ass "y" (Mult (V "z") (V "y"))))))] (Comp (Ass "y" (N 1)) (Call "fac")))

-- A mututally recursive program to determine if the value of "x" is odd or
-- even. If "x" is even, "y" is populated with 0, otherwise "y" is populated
-- with 1.
mutualEvenOdd :: Stm
mutualEvenOdd = (Block [] [("isEven",If (Eq (V "x") (N 0)) (Ass "y" (N 0)) (Comp (Ass "x" (Sub (V "x") (N 1))) (Call "isOdd"))),("isOdd",If (Eq (V "x") (N 0)) (Ass "y" (N 1)) (Comp (Ass "x" (Sub (V "x") (N 1))) (Call "isEven")))] (Call "isEven"))
aValSpec :: Spec
aValSpec = do
    describe "aVal" $ do
        it "evaulates numerals" $ do
            aVal (N 5) testState `shouldBe` 5

        it "evaulates variable x" $ do
            aVal (V "x") testState `shouldBe` 1

        it "evaulates variable y" $ do
            aVal (V "y") testState `shouldBe` 2

        it "evaulates variable z" $ do
            aVal (V "z") testState `shouldBe` 3

        it "evaulates multiplication" $ do
            aVal (Mult (V "y") (N 5)) testState `shouldBe` 10

        it "evaulates chained multiplication" $ do
            aVal (Mult (Mult (N 2) (N 3)) (V "z")) testState `shouldBe` 18

        it "evaulates addition" $ do
            aVal (Add (N 2) (N 8)) testState `shouldBe` 10

        it "evaulates chained addition" $ do
            aVal (Add (Add (V "x") (V "y")) (V "z")) testState `shouldBe` 6

        it "evaulates subtraction" $ do
            aVal (Sub (N 2) (N 8)) testState `shouldBe` -6

        it "evaulates chained subtraction" $ do
            aVal (Sub (Sub (V "x") (V "y")) (V "z")) testState `shouldBe` -4

bValSpec :: Spec
bValSpec = do
    describe "bVal" $ do
        it "evaluates TRUE" $ do
            bVal TRUE testState `shouldBe` True

        it "evaulates FALSE" $ do
            bVal FALSE testState `shouldBe` False

        it "evaluates negation of true" $ do
            bVal (Neg TRUE) testState `shouldBe` False

        it "evaulates negation of false" $ do
            bVal (Neg FALSE) testState `shouldBe` True

        it "evaulates FALSE & FALSE" $ do
            bVal (And FALSE FALSE) testState `shouldBe` False

        it "evaulates FALSE & TRUE" $ do
            bVal (And FALSE TRUE) testState `shouldBe` False

        it "evaulates TRUE & FALSE" $ do
            bVal (And TRUE FALSE) testState `shouldBe` False

        it "evaulates TRUE & TRUE" $ do
            bVal (And TRUE TRUE) testState `shouldBe` True

        it "evaulates TRUE & TRUE & FALSE" $ do
            bVal (And (And TRUE TRUE) FALSE) testState `shouldBe` False

        it "evaulates <= to be true" $ do
            bVal (Le (V "y") (N 6)) testState `shouldBe` True

        it "evaulates <= to be false" $ do
            bVal (Le (N 7) (V "z")) testState `shouldBe` False

        it "evaulates == to be true" $ do
            bVal (Eq (V "x") (N 1)) testState `shouldBe` True

        it "evaulates == to be false" $ do
            bVal (Eq (V "x") (V "y")) testState `shouldBe` False

updateSpec :: Spec
updateSpec = do
    describe "update" $ do
        it "updates the returned value" $ do
            update 5 "x" testState "x" `shouldBe` 5

        it "does not change the value of non-updated variables" $ do
            update 5 "x" testState "y" `shouldBe` 2
            update 5 "x" testState "z" `shouldBe` 3

updateManySpec :: Spec
updateManySpec = do
    describe "updateMany" $ do
        it "updates the returned values" $ do
             keepUpdating [("x", 5), ("y", 6)] testState "x" `shouldBe` 5
             keepUpdating [("x", 5), ("y", 6)] testState "y" `shouldBe` 6

        it "does not change the value of non-updated variables" $ do
            keepUpdating [("x", 5), ("y", 6)] testState "z" `shouldBe` 3

restoreSpec :: Spec
restoreSpec = do
    describe "restores" $ do
        it "updates the returned values" $ do
            restore newS ( ["a", "b"]) oldS "a" `shouldBe` 5
            restore newS (["a", "b"]) oldS "b" `shouldBe` 6

        it "does not change the value of non-updated variables" $ do
            restore newS ( ["a"]) oldS "b" `shouldBe` 2 where

        newS :: State
        newS "a" = 1
        newS "b" = 2

        oldS :: State
        oldS "a" = 5
        oldS "b" = 6

sDynamicSpec :: Spec
sDynamicSpec = do
    describe "s_dynamic" $ do
        it "skips" $ do
            s_dynamic Skip testState "x" `shouldBe` 1
            s_dynamic Skip testState "y" `shouldBe` 2
            s_dynamic Skip testState "z" `shouldBe` 3

        it "assigns" $ do
            s_dynamic (Ass "x" (N 5)) testState "x" `shouldBe` 5
            s_dynamic (Ass "x" (N 5)) testState "y" `shouldBe` 2
            s_dynamic (Ass "x" (N 5)) testState "z" `shouldBe` 3

        it "performs s1 if IF predicate is true" $ do
            s_dynamic (If TRUE (Ass "x" (N 5)) (Ass "x" (N 0))) testState "x" `shouldBe` 5

        it "performs s2 if IF predicate is false" $ do
            s_dynamic (If FALSE (Ass "x" (N 5)) (Ass "x" (N 0))) testState "x" `shouldBe` 0

        it "does composition" $ do
            s_dynamic (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "x" `shouldBe` 5
            s_dynamic (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "y" `shouldBe` 6
            s_dynamic (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "z" `shouldBe` 3

        it "does not perform the body of the while loop of the condition is false" $ do
            s_dynamic (While FALSE (Ass "x" (N 5))) testState "x" `shouldBe` 1

        it "does while loops" $ do
            s_dynamic (While (Neg (Eq (V "z") (N 0))) (Ass "z" (Sub (V "z") (N 1)))) testState "z" `shouldBe` 0

        it "correctly evaulates program" $ do
            s_dynamic scopeStm emptyState "y" `shouldBe` 6

        it "evaulates with self-recursion" $ do
            s_dynamic recursiveFac (update  1"x"  emptyState) "y" `shouldBe` 1
            s_dynamic recursiveFac (update 2 "x"  emptyState) "y" `shouldBe` 2
            s_dynamic recursiveFac (update 3 "x"  emptyState) "y" `shouldBe` 6
            s_dynamic recursiveFac (update 4 "x"  emptyState) "y" `shouldBe` 24
            s_dynamic recursiveFac (update 5 "x"  emptyState) "y" `shouldBe` 120

        it "evaulates with mutual-recursion" $ do
            s_dynamic mutualEvenOdd (update 0 "x"  emptyState) "y" `shouldBe` 0
            s_dynamic mutualEvenOdd (update 1 "x"  emptyState) "y" `shouldBe` 1
            s_dynamic mutualEvenOdd (update 2 "x"  emptyState) "y" `shouldBe` 0
            s_dynamic mutualEvenOdd (update 3 "x"  emptyState) "y" `shouldBe` 1
            s_dynamic mutualEvenOdd (update 4 "x"  emptyState) "y" `shouldBe` 0



sMixedSpec :: Spec
sMixedSpec = do
    describe "s_mixed" $ do
        it "skips" $ do
            s_mixed Skip testState "x" `shouldBe` 1
            s_mixed Skip testState "y" `shouldBe` 2
            s_mixed Skip testState "z" `shouldBe` 3

        it "assigns" $ do
            s_mixed (Ass "x" (N 5)) testState "x" `shouldBe` 5
            s_mixed (Ass "x" (N 5)) testState "y" `shouldBe` 2
            s_mixed (Ass "x" (N 5)) testState "z" `shouldBe` 3

        it "performs s1 if IF predicate is true" $ do
            s_mixed (If TRUE (Ass "x" (N 5)) (Ass "x" (N 0))) testState "x" `shouldBe` 5

        it "performs s2 if IF predicate is false" $ do
            s_mixed (If FALSE (Ass "x" (N 5)) (Ass "x" (N 0))) testState "x" `shouldBe` 0

        it "does composition" $ do
            s_mixed (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "x" `shouldBe` 5
            s_mixed (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "y" `shouldBe` 6
            s_mixed (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "z" `shouldBe` 3

        it "does not perform the body of the while loop of the condition is false" $ do
            s_mixed (While FALSE (Ass "x" (N 5))) testState "x" `shouldBe` 1

        it "does while loops" $ do
            s_mixed (While (Neg (Eq (V "z") (N 0))) (Ass "z" (Sub (V "z") (N 1)))) testState "z" `shouldBe` 0

        it "correctly evaulates program" $ do
            s_mixed scopeStm emptyState "y" `shouldBe` 10

        it "evaulates with self-recursion" $ do
            s_mixed recursiveFac (update 1 "x"  emptyState) "y" `shouldBe` 1
            s_mixed recursiveFac (update 2 "x"  emptyState) "y" `shouldBe` 2
            s_mixed recursiveFac (update 3 "x"  emptyState) "y" `shouldBe` 6
            s_mixed recursiveFac (update 4 "x"  emptyState) "y" `shouldBe` 24
            s_mixed recursiveFac (update 5 "x"  emptyState) "y" `shouldBe` 120

        it "evaulates with mutual-recursion" $ do
            s_mixed mutualEvenOdd (update 0 "x"  emptyState) "y" `shouldBe` 0
            s_mixed mutualEvenOdd (update 1 "x"  emptyState) "y" `shouldBe` 1
            s_mixed mutualEvenOdd (update 2 "x"  emptyState) "y" `shouldBe` 0
            s_mixed mutualEvenOdd (update 3 "x"  emptyState) "y" `shouldBe` 1
            s_mixed mutualEvenOdd (update 4 "x"  emptyState) "y" `shouldBe` 0


sStaticSpec :: Spec
sStaticSpec = do
    describe "s_static" $ do
        it "skips" $ do
            s_static Skip testState "x" `shouldBe` 1
            s_static Skip testState "y" `shouldBe` 2
            s_static Skip testState "z" `shouldBe` 3

        it "assigns" $ do
            s_static (Ass "x" (N 5)) testState "x" `shouldBe` 5
            s_static (Ass "x" (N 5)) testState "y" `shouldBe` 2
            s_static (Ass "x" (N 5)) testState "z" `shouldBe` 3

        it "performs s1 if IF predicate is true" $ do
            s_static (If TRUE (Ass "x" (N 5)) (Ass "x" (N 0))) testState "x" `shouldBe` 5

        it "performs s2 if IF predicate is false" $ do
            s_static (If FALSE (Ass "x" (N 5)) (Ass "x" (N 0))) testState "x" `shouldBe` 0

        it "does composition" $ do
            s_static (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "x" `shouldBe` 5
            s_static (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "y" `shouldBe` 6
            s_static (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "z" `shouldBe` 3

        it "does not perform the body of the while loop of the condition is false" $ do
            s_static (While FALSE (Ass "x" (N 5))) testState "x" `shouldBe` 1

        it "does while loops" $ do
            s_static (While (Neg (Eq (V "z") (N 0))) (Ass "z" (Sub (V "z") (N 1)))) testState "z" `shouldBe` 0

        it "correctly evaulates program" $ do
            s_static scopeStm emptyState "y" `shouldBe` 5

        it "evaulates with self-recursion" $ do
            s_static recursiveFac (update  1"x"  emptyState) "y" `shouldBe` 1
            s_static recursiveFac (update 2 "x"  emptyState) "y" `shouldBe` 2
            s_static recursiveFac (update 3 "x"  emptyState) "y" `shouldBe` 6
            s_static recursiveFac (update 4 "x"  emptyState) "y" `shouldBe` 24
            s_static recursiveFac (update 5 "x"  emptyState) "y" `shouldBe` 120

        it "evaulates with mutual-recursion" $ do
            s_static mutualEvenOdd (update 0 "x"  emptyState) "y" `shouldBe` 0
            s_static mutualEvenOdd (update 1 "x"  emptyState) "y" `shouldBe` 1
            s_static mutualEvenOdd (update 2 "x"  emptyState) "y" `shouldBe` 0
            s_static mutualEvenOdd (update 3 "x"  emptyState) "y" `shouldBe` 1
            s_static mutualEvenOdd (update 4 "x"  emptyState) "y" `shouldBe` 0
