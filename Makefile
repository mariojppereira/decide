BIN=decide

all: $(BIN)

$(BIN): decide.ml
	ocamlfind ocamlopt -o $(BIN) $< -package yojson -linkpkg

PHONY: run clean

run:: $(BIN)
	./run_examples.sh

clean::
	rm -f *.cm[xi] *.o *~ $(BIN)
