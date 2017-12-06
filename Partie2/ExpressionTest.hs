import Test.HUnit
import Expression

tests = TestList [  TestLabel "testFindVarNotOK1" test1,
                    TestLabel "testFindVarNotOK2" test2,
                    TestLabel "testFindVarOK1" test3,
                    TestLabel "testFindVarOK2" test4,
                    TestLabel "testEvalNotOK" test5,
                    TestLabel "testEvalOK" test6,
                    TestLabel "testEvalOK" test7]

------------------------------------Test findVar
test1 = TestCase (assertEqual "find var x" (Nothing) (findVar [] "x"))
test2 = TestCase (assertEqual "find var x" (Nothing) (findVar [("y",1)] "x"))

test3 = TestCase (assertEqual "find var x" (Just 1.0) (findVar [("x",1)] "x"))
test4 = TestCase (assertEqual "find var x" (Just 2.0) (findVar [("y",1), ("x",2)] "x"))

------------------------------------Test eval

e1 = Bin "+" (Bin "*" (Bin "+" (Const 5.0) (Const 3.0)) (Const 18.0)) (Uni "-" (Const 4.0))
e2 = Bin "+" (Bin "*" (Bin "+" (Variable "a") (Const 3.0)) (Const 18.0)) (Uni "-" (Const 4.0))

test5 = TestCase (assertEqual "eval with var " (Nothing) (eval [] e2))
test6 = TestCase (assertEqual "eval with var" (Just 140) (eval [("a", 5)] e2))
test7 = TestCase (assertEqual "eval without var" (Just 140) (eval [] e1))