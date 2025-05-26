run:
	./bin/Jeu_min_max.exe

compile:
	@mkdir -p build
	@mkdir -p bin
	cargo build --target x86_64-pc-windows-gnu --target-dir build
	@cp build/x86_64-pc-windows-gnu/debug/Jeu_min_max.exe bin/Jeu_min_max.exe
	@make clean || true

compile_run: compile run

clean:
	@-rm -rf build/*

