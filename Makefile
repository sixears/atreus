.PHONY: ff

atreus=dist-newstyle/build/x86_64-linux/ghc-8.8.4/atreus-1.0.0.0/x/atreus/build/atreus/atreus

ff: atreus.svg
	firefox $^

$(atreus): bin/atreus.hs src/Atreus/LayoutDiagram.hs atreus.cabal
	nabal v2-build

atreus.svg: $(atreus)
	$(atreus) -o $@ -w 400
