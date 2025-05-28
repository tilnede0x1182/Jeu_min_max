DOTNET = /mnt/c/Program\ Files/dotnet/dotnet.exe

run:
	./bin/Jeu_min_max.exe

compile:
	@mkdir -p bin
	$(DOTNET) build src -c Release -o bin

compile_run: compile run

clean:
	rm -rf src/bin src/obj
