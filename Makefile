.PHONY: all
all:
	cd src && ocamlopt -o ../finite-automata util.ml DFA.ml main.ml

.PHONY: bytecode
bytecode:
	cd src && ocamlc -o ../finite-automata util.ml DFA.ml main.ml
