CABAL=cabal
all:
	$(CABAL) update
	$(CABAL) install -f -graphviz --only-dependencies
	$(CABAL) configure -f -graphviz
	$(CABAL) build

.PHONY: all

