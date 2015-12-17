CABAL=cabal
RUNTIME=src/runtime/
GC=src/GC/

all: compiler

compiler:
	$(CABAL) update
	$(CABAL) install fgl optparse-applicative
	$(CABAL) configure -f -graphviz
	$(CABAL) build

runtime:
	$(MAKE) -C $(RUNTIME)
gc:
	$(MAKE) -C $(GC)
clean:
	cabal clean
	$(MAKE) -C $(RUNTIME) clean
	$(MAKE) -C $(GC) clean
	
.PHONY: all clean
