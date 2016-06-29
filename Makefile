BIN=decide

all: $(BIN)

$(BIN): decide.ml
	ocamlfind ocamlopt -o $(BIN) $< -package yojson -linkpkg

PHONY: test clean

test:: $(BIN)
	./$(BIN) input-DECIDE/input0.json

clean::
	rm -f *.cm[xi] *.o *~ $(BIN)
