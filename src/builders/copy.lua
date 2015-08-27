local unix_copy = {
	bin = 'cp',
	flags = '-r',
	cmd = '$bin $flags $input $output'
}

local win_copy = {
	bin = 'copy',
	cmd = '$bin $input $output'
}

copy = Builder ((hell.os.name == 'windows' and win_copy) or unix_copy)
copy.help = "Just copies inputs (one by one)\
When using `util.glob` in inputs, better set `multinput = true'\
If no outDir specified, uses \"copy_of_<your file name>\" as output name\
Default flag: -r (for copying folders)"
