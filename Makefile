.PHONY: all
all:
	cd src && ocamlopt -o ../finite-automata util.ml DFA.ml main.ml
	$(MAKE) clean

.PHONY: bytecode
bytecode:
	cd src && ocamlc -o ../finite-automata util.ml DFA.ml main.ml
	$(MAKE) clean

.PHONY: clean
clean:
	rm -f {src,tests}/*.cmi
	rm -f {src,tests}/*.cmo
	rm -f {src,tests}/*.cmx
	rm -f {src,tests}/*.o
