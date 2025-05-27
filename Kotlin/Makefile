run:
	java -jar build_jar/MinMax.jar

compile:
	@mkdir -p ./build_jar
	kotlinc src/MinMax.kt -include-runtime -d build_jar/MinMax.jar

compile_run: compile run
