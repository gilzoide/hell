-- default C++ builder (cuz it's the only one we have), just as C
cxx = gcc:extend {
	bin = 'g++',
	help = "Compiles a C++ program, pipeBuilding all of the input files as objects first"
}

cxx.obj = gcc.obj:extend {
	bin = 'g++',
	help = "Compiles a C++ object"
}

cxx.shared = gcc.shared:extend {
	bin = 'g++',
	help = "Compiles a C++ shared library, pipeBuilding all of the input files as PIC objects first"
}

-- C++ sources have many file extensions, just cover them
cpp = cxx
cc = cxx
