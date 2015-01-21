hell.help = "e então, minha gente..."

a = {
	gcc {
		input = 'oi.c',
		links = 'sdl'
	},
	install {
		input = 'readme.txt'
	}
}

build {
	input = 'outro.c',
	output = 'oUtRo'
}

feedHellFire ('mais_um_teste.lua', _ENV)

install (a[1])
