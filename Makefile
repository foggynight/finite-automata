SRC_DIR = src
SRC_FILES = util.ml NFA.ml DFA.ml parser.ml main.ml
SRCS = $(addprefix $(SRC_DIR)/,$(SRC_FILES))

.PHONY: all
all:
	ocamlopt -o finite-automata -I $(SRC_DIR) $(SRCS)
	$(MAKE) clean

.PHONY: bytecode
bytecode:
	ocamlc -o finite-automata -I $(SRC_DIR) $(SRCS)
	$(MAKE) clean

.PHONY: clean
clean:
	rm -f {src,tests}/*.cmi
	rm -f {src,tests}/*.cmo
	rm -f {src,tests}/*.cmx
	rm -f {src,tests}/*.o
