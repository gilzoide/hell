java = Builder {
	bin = 'javac',
	flags = '',
	sourcepath = false,
	cmd = '$bin $flags $sourcepath $input'
}

function java.prepare_sourcepath (srcpth, input)
	return srcpth and '-sourcepath ' .. srcpth or ''
end

function java.prepare_output (_, input)
	return utils.changeExtension ('class', input)
end

function java.prepare_flags (flags, input)
	return (flags or '') .. (hell.outdir and ' -d ' .. hell.outdir or '')
end
