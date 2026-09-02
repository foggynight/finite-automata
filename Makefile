.PHONY: all
all:
	cd src && ocamlopt -o ../finite-automata util.ml DFA.ml main.ml

.PHONY: bytecode
bytecode:
	cd src && ocamlc -o ../finite-automata util.ml DFA.ml main.ml

.PHONY: clean
clean:
	rm -f {src,test}/*.cmi
	rm -f {src,test}/*.cmo
	rm -f {src,test}/*.cmx
	rm -f {src,test}/*.o
