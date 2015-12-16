local utils = hell.utils

java = Builder {
	bin = 'javac',
	flags = '',
	sourcepath = false,
	classpath = false,
	cmd = '$bin $flags $classpath $sourcepath $input'
}

function java.prepare_sourcepath (srcpth, input)
	return srcpth and '-sourcepath ' .. srcpth or ''
end

function java.prepare_classpath (classpath, input)
	return classpath and '-cp ' .. utils.concat (classpath, ':') or ''
end

function java.prepare_output (_, input)
	return utils.changeExtension ('class', input)
end

function java.prepare_flags (flags, input)
	return (flags or '') .. (hell.outdir and ' -d ' .. hell.outdir or '')
end
