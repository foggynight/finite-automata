SRCS = util.ml DFA.ml NFA.ml main.ml

.PHONY: all
all:
	cd src && ocamlopt -o ../finite-automata $(SRCS)
	$(MAKE) clean

.PHONY: bytecode
bytecode:
	cd src && ocamlc -o ../finite-automata $(SRCS)
	$(MAKE) clean

.PHONY: clean
clean:
	rm -f {src,tests}/*.cmi
	rm -f {src,tests}/*.cmo
	rm -f {src,tests}/*.cmx
	rm -f {src,tests}/*.o
