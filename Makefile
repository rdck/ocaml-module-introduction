functors: functors.ml
	ocamlfind ocamlopt -o functors -linkpkg -package core functors.ml

.PHONY: clean

clean:
	rm -f functors *.cmi *.cmx *.o
