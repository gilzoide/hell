local util = hell.utils

java = Builder {
	bin = 'javac',
	flags = '',
	sourcepath = false,
	multinput = true,
	cmd = '$bin $flags $sourcepath $input'
}

function java.prepare_sourcepath (srcpth, input)
	return srcpth and '-sourcepath ' .. srcpth or ''
end

function java.prepare_output (out, input)
	return util.changeExtension (input, 'class')
end

function java.prepare_flags (f, input)
	return (f or '') .. (hell.outdir and ' -d ' .. hell.outdir or '')
end
