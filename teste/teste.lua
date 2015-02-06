hell.help = "e então, minha gente..."
hell.outdir = "build"

oi = build {
	input = 'outro.c',
	output = 'oUtRo'
}

a = {
	gcc {
		input = 'oi.c',
		links = 'sdl',
		deps = {oi}
	},
	install {
		input = 'readme.txt'
	}
}

feedHellFire ('recursive/mais_um_teste.lua')

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

e = {}

install (a[1])
