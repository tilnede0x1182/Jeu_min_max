DOTNET = /mnt/c/Program\ Files/dotnet/dotnet.exe

run:
	./bin/Jeu_min_max.exe

compile:
	@mkdir -p build
	@mkdir -p bin
	$(DOTNET) new console -o build --force --name Jeu_min_max
	@rm build/Program.cs
	@cp src/*.cs build/
	$(DOTNET) publish build/Jeu_min_max.csproj -c Release -r win-x64 --self-contained false -o bin
	@make clean

compile_run: compile run

clean:
	@rm -rf build/*
	# @find bin -type f ! -name 'Jeu_min_max.exe' -delete
	# @find bin -type d -empty -delete

