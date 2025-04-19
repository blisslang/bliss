default: compile

compile:
	mkdir -p build
	dart compile exe bin/bliss.dart -o build/bliss.exe