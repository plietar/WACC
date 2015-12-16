CABAL=cabal
RUNTIME=src/runtime/
all:
#	$(CABAL) update
#	$(CABAL) install fgl optparse-applicative
#	$(CABAL) configure -f -graphviz
	$(CABAL) build
	$(MAKE) -C $(RUNTIME)

.PHONY: all

