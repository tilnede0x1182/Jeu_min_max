run:
	./bin/Jeu_min_max.exe

compile:
	@rm bin/Jeu_min_max.exe
	@mkdir -p ~/tmp
	@mkdir -p bin
	cargo build --target x86_64-pc-windows-gnu --target-dir ~/tmp/rust_tmp
	@mv ~/tmp/rust_tmp/x86_64-pc-windows-gnu/debug/Jeu_min_max.exe bin/Jeu_min_max.exe

compile-prod:
	@rm bin/Jeu_min_max.exe
	@mkdir -p ~/tmp
	@mkdir -p bin
	cargo build --release --target x86_64-pc-windows-gnu --target-dir ~/tmp/rust_tmp
	@mv ~/tmp/rust_tmp/x86_64-pc-windows-gnu/release/Jeu_min_max.exe bin/Jeu_min_max.exe

compile_run: compile run

clean:
	@-rm -rf build/*
