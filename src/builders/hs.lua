hs = Builder {
	bin = 'ghc',
	flags = '--make',
	prepare_flags = utils.curry (utils.prefix, hell.outdir and ' -outputdir ' .. hell.outdir or ''),
	prepare_input = utils.concat,
	prepare_output = function (out, input)
		return out or utils.changeExtension (hell.os.exe_ext, input)
	end,
	cmd = '$bin $flags $input -o $output'
}
