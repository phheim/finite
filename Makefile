BLDTOOL=`if [ -d "dist" ]; then echo "cabal"; else echo "stack"; fi`

default:
	${BLDTOOL} build

ghci:
	${BLDTOOL} repl

install:
	${BLDTOOL} install

haddock:
	${BLDTOOL} haddock

test:
	${BLDTOOL} test

clean:
	${BLDTOOL} clean

.PHONY: clean
.SILENT:
