.PHONY: test

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
