CABAL=cabal
all:
	$(CABAL) update
	$(CABAL) install fgl optparse-applicative
	$(CABAL) configure -f -graphviz
	$(CABAL) build

.PHONY: all

