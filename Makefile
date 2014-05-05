main: main.hs
	ghc -XExistentialQuantification main.hs

tests := $(wildcard tests/*.ss)

test: main $(tests)
	./test.sh
