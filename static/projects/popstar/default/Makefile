.PHONY: test

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f popstar.zip
	zip -r popstar.zip . -x@exclude.lst

clean:
	dune clean
	rm -f popstar.zip

doc:
	dune build @doc
