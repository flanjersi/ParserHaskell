
all: EnvInteractif ExpressionTest ParseTest

EnvInteractif: Parse.hs Expression.hs
	ghc --make EnvInteractif.hs

ParseTest: Parse.hs Expression.hs
	ghc --make ParseTest.hs

ExpressionTest: Expression.hs
	ghc --make ExpressionTest.hs
        

clean:
	- rm -f EnvInteractif ParseTest ExpressionTest
	- rm -f *.o
	- rm -f *.hi
    