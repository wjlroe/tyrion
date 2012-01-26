all: deps configure compile test

configure: deps
	@runhaskell Setup.lhs configure --user

deps:
	@cabal install --only-dependencies

compile: configure
	@runhaskell Setup.lhs build

test: compile
	cd tests && make
