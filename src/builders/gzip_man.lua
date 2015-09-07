gzip = Builder {
	prepare_output = function (_, i) return i .. '.gz' end,
	flags = nil,
	cmd = 'gzip -c $flags $input > $output',
	help = "Compress something with gzip"
}

man = gzip:extend {
	help = "Create a man page by gzipping it"
}
