

run:
	@if [ -f build_jar/MinMax.jar ]; then \
		java -jar build_jar/MinMax.jar; \
	elif [ -f build_classes/MinMax.class ]; then \
		java build_classes/MinMax; \
	else \
		javac -d build_classes src/MinMax.java && java build_classes/MinMax; \
	fi

compile:
	@mkdir -p ./build_classes
	javac -d build_classes src/MinMax.java

compile_run: compile run

jar: compile
	@mkdir -p ./build_jar
	jar cfe build_jar/MinMax.jar MinMax -C build_classes .

javadoc:
	@mkdir -p ./javadoc
	javadoc -d javadoc src/MinMax.java

run_jar: jar run
