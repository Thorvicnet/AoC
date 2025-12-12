all: build
	./main
clean:
	rm *.cmi *.cmx *.o
build:
	ocamlfind ocamlopt -package re -c prelude.mli
	ocamlfind ocamlopt -package re -c prelude.ml
	ocamlfind ocamlopt -package re -linkpkg -o main prelude.cmx main.ml
