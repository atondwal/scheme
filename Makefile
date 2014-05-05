main: main.hs
	ghc main.hs

tests := $(wildcard tests/*.ss)

test: main $(tests)
	./test.sh
