CABAL=cabal
all:
	$(CABAL) update
	$(CABAL) configure
	$(CABAL) build

.PHONY: all

