hell.help = "e então, minha gente..."
hell.outdir = "../teste"

oi = build {
	input = 'outro.c',
	includes = 'include',
	output = 'oUtRo',
}

cara = build {
	input = 's.c',
    deps = {oi}
}

oi:addDep (cara)

a = {
	build {
		input = 'oi.c',
		links = 'sdl',
		deps = {cara}
	},
	c.shared {
		input = 'shared.c'
	},
	install ({
		input = 'readme.txt'
	}, 'lib')
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

install (a[1], 'bin')
