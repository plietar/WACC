CABAL=cabal
all:
	$(CABAL) update
	$(CABAL) install --only-dependencies
	$(CABAL) configure
	$(CABAL) build

.PHONY: all

