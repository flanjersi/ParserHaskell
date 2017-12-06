
all: EnvInteractif

EnvInteractif: Parse.hs Expression.hs
		ghc --make EnvInteractif.hs

clean:
	- rm -f EnvInteractif
	- rm -f *.o
	- rm -f *.hi
