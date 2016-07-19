local utils = hell.utils

java = Builder {
	bin = 'javac',
	flags = nil,
	sourcepath = nil,
	classpath = nil,
	cmd = '$bin $outdir $flags $classpath $sourcepath $input',
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

function java.prepare_outdir (outdir, input)
	local outdir = outdir or hell.outdir
	return outdir and '-d ' .. outdir or ''
end


-- JAR Builder
jar = Builder {
	bin = 'jar',
	flags = 'cf',
	cmd = "$bin $flags $output $input",
	help = [[
Java JAR file creator

To insert custom manifest, set `flags = '&m <your_manifest>'
]],
}

function jar.prepare_output (output, input)
	return utils.changeExtension ('jar', input)
end
