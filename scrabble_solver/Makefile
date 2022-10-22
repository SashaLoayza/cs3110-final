.PHONY: test

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop

test:
	OCAMLRUNPARAM=b dune test 

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
