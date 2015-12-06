-- default C++ builder (cuz it's the only one we have), just as C
cpp = gcc:extend {
	bin = 'g++',
	help = "Compiles a C++ program, pipeBuilding all of the input files as objects first"
}

cpp.obj = gcc.obj:extend {
	bin = 'g++',
	help = "Compiles a C++ object"
}

cpp.shared = gcc.shared:extend {
	bin = 'g++',
	help = "Compiles a C++ shared library, pipeBuilding all of the input files as PIC objects first"
}
