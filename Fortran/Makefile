run:
	./bin/Jeu_min_max.exe

compile:
	@mkdir -p build
	@mkdir -p bin
	gfortran -c src/Jeu_min_max.f90 -J build -o build/Jeu_min_max.o
	gfortran build/Jeu_min_max.o -o bin/Jeu_min_max.exe
	@rm -rf build/*

compile_run: compile run
