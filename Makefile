CABAL=cabal
all:
	$(CABAL) configure
	$(CABAL) build

.PHONY: all

