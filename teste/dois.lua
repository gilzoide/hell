-- teste do glob

hell.outdir = 'build'

a = hell.utils.glob ("../**/*.lua")

build {
	input = a
}
