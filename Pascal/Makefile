run:
	./bin/Jeu_min_max.exe

compile:
	@mkdir -p bin
	@mkdir -p build
	fpc -FEbuild -FUbuild -obin/Jeu_min_max.exe src/Jeu_min_max.pas
	@rm -rf build/*

compile_run: compile run
