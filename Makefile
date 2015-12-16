CABAL=cabal
RUNTIME=src/runtime/

all: compiler

compiler:
	$(CABAL) update
	$(CABAL) install fgl optparse-applicative
	$(CABAL) configure -f -graphviz
	$(CABAL) build

runtime:
	$(MAKE) -C $(RUNTIME)
	
.PHONY: all
