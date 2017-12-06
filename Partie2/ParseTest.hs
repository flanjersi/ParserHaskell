import Test.HUnit
import Parse
import Expression

tests = TestList [  TestLabel "testParsePar" test1,
                    TestLabel "testParseNothing" test2,
                    TestLabel "testParseErrorConst" test3,
                    TestLabel "testParseErrorVar" test4,
                    TestLabel "testParseExpr" test5,
                    TestLabel "testParseExprVar" test6]

------------------------------------Test parseExpression
test1 = TestCase (assertEqual "parse ()" (Nothing) (parseExpression "()"))
test2 = TestCase (assertEqual "parse " (Nothing) (parseExpression ""))
test3 = TestCase (assertEqual "parse 1.a" (Nothing) (parseExpression "1.2.a"))
test4 = TestCase (assertEqual "parse a" (Just (Variable "a")) (parseExpression "a"))

e1 = "(5+3) * 18 + sin 4"
e2 = "(b+3) * 18 + sin 4"

expr1 = Just (Bin "+" (Bin "*" (Bin "+" (Const 5.0) (Const 3.0)) (Const 18.0)) (Uni "sin" (Const 4.0)))
expr2 = Just (Bin "+" (Bin "*" (Bin "+" (Variable "b") (Const 3.0)) (Const 18.0)) (Uni "sin" (Const 4.0)))

test5 = TestCase (assertEqual "parse expr without var" (expr1) (parseExpression e1))
test6 = TestCase (assertEqual "parse expr with var" (expr2) (parseExpression e2))