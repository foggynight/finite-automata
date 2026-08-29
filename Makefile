.PHONY: all
all:
	cd src && ocamlopt -o ../finite-automata util.ml DFA.ml main.ml
