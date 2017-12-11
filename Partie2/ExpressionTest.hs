module Main where

import Test.HUnit
import Expression

tests = TestList [  TestLabel "testValueVarNotOK1" testS1,
                    TestLabel "testValueVarNotOK2" testS2,
                    TestLabel "testValueVarOK1" testS3,
                    TestLabel "testValueVarOK2" testS4,
                    TestLabel "testAddVarOK" testS5,
                    TestLabel "testRemoveVarOK" testS6,
                    TestLabel "testEvalNotOK" testE1,
                    TestLabel "testEvalOK" testE2,
                    TestLabel "testEvalOK" testE3
                 ]

------------------------------------Test store
testS1 = TestCase (assertEqual "value var x" (Left "La variable x n'existe pas") (valueVar [] "x"))
testS2 = TestCase (assertEqual "value var x" (Left "La variable x n'existe pas") (valueVar [("y",1)] "x"))

testS3 = TestCase (assertEqual "value var x" (Right 1.0) (valueVar [("x",1)] "x"))
testS4 = TestCase (assertEqual "value var x" (Right 2.0) (valueVar [("y",1), ("x",2)] "x"))

testS5 = TestCase (assertEqual "value var x" (Right 1.0) (valueVar (addToStore [] "x" 1) "x"))
testS6 = TestCase (assertEqual "value var x" (Left "La variable x n'existe pas") (valueVar (removeFromStore [("y",1), ("x",2)] "x") "x"))

------------------------------------Test eval

e1 = Bin "+" (Bin "*" (Bin "+" (Const 5.0) (Const 3.0)) (Const 18.0)) (Uni "sin" (Const 4.0))
e2 = Bin "+" (Bin "*" (Bin "+" (Variable "a") (Const 3.0)) (Const 18.0)) (Uni "sin" (Const 4.0))

testE1 = TestCase (assertEqual "eval with var " (Left "Une variable n'a pas été trouvé") (eval [] e2))
testE2 = TestCase (assertEqual "eval with var" (Right 143.2432) (eval [("a", 5)] e2))
testE3 = TestCase (assertEqual "eval without var" (Right 143.2432) (eval [] e1))


main = runTestTT tests