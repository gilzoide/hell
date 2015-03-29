hs = Builder {
	bin = 'ghc',
	flags = '--make',
	prepare_flags = function (f)
		local outdir = hell.outdir and ' -outputdir ' .. hell.outdir or ''
		return f .. outdir
	end,
	prepare_input = hell.utils.concat,
	prepare_output = function (out, input)
		return out or util.changeExtension (input, hell.os.exe_ext)
	end,
	cmd = '$bin $flags $input -o $output'
}
