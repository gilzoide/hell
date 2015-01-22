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

feedHellFire ('mais_um_teste.lua')

-- target aninhada!
a.c = {
	b[1],
	build {
		input = 'palhaço',
		echo = 'me copia'
	},
	d = {
		build {
			input = 'eita, chega!'
		}
	}
}

install (a[1])
