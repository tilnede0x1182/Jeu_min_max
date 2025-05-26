compile:
	@mkdir -p ./build
	ocamlopt unix.cmxa src/Jeu_min_max.ml -o build/Jeu_min_max.exe
	@rm -f src/*.cmi src/*.cmo src/*.cmx src/*.o

run:
	./build/Jeu_min_max.exe

compile_run: compile run
