PDF_VIEWER := evince
GRAPH_TYPE := -hc

centres: GRAPH_TYPE := -hc
centres: benchmark

modules: GRAPH_TYPE := -hm
modules: benchmark

descs: GRAPH_TYPE := -hd
descs: benchmark

types: GRAPH_TYPE := -hy
types: benchmark

benchmark:
	cabal run 2 ngrams.arpa text.txt 6 50 -- +RTS $(GRAPH_TYPE) -i0.1
	hp2ps -e9in -c NGramPredict.hp
	epstopdf NGramPredict.ps
	$(PDF_VIEWER) NGramPredict.pdf &

help:
	@echo "Usage: make <benchmark type>"
	@echo "Available types:"
	@echo "    \"\"      ... default benchmark -> cost centres"
	@echo "    centres ... cost centre benchmark"
	@echo "    modules ... module benchmark"
	@echo "    descs   ... closure description benchmark (e.g. constructor names)"
	@echo "    types   ... data type benchmark"
	@echo "\nThis will run the executable, create a pdf of the benchmark,"
	@echo "and then open it with a pdf viewer (evince is the default)"
