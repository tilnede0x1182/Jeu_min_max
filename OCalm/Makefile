run:
	./bin/Jeu_min_max.exe

compile:
	@mkdir -p ./bin
	ocamlopt unix.cmxa src/Jeu_min_max.ml -o bin/Jeu_min_max.exe
	@rm -f src/*.cmi src/*.cmo src/*.cmx src/*.o

compile_run: compile run
