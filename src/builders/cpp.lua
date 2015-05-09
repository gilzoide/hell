-- default C++ builder (cuz it's the only one we have), just as C
cpp = gcc:extend {
	bin = 'g++'
}

cpp.shared = gcc.shared:extend {
	bin = 'g++'
}
