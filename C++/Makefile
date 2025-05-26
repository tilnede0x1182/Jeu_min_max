run:
	./bin/Jeu_min_max.exe

compile:
	@mkdir -p build
	@mkdir -p bin
	g++ -std=c++17 -Wall -Wextra -I./src -c src/Jeu_min_max.cpp -o build/Jeu_min_max.o
	g++ build/Jeu_min_max.o -o bin/Jeu_min_max.exe
	@rm -f build/*.o

compile_run: compile run
