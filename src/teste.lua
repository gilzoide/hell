hell.help = "e então, minha gente..."

a = gcc {
	input = 'oi.c',
	links = 'sdl'
}

build {
	input = 'outro.c',
	output = 'oUtRo'
}

install (a)
