run:
	./bin/Jeu_min_max.exe

compile:
	@mkdir -p build
	@mkdir -p bin
	gcc -c src/Jeu_min_max.c -o build/Jeu_min_max.o
	gcc build/Jeu_min_max.o -o bin/Jeu_min_max.exe
	@rm -rf build/*

compile_run: compile run
