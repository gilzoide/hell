local util = require 'hellutils'

local unix_copy = {
	bin = 'cp',
	flags = 'r',
	prepare_flags = util.curryPrefixEach ('-'),
	cmd = '$bin $flags $input $output'
}

local win_copy = {
	bin = 'copy',
	cmd = '$bin $input $output'
}

copy = Builder ((hell.os.name == 'unix' and unix_copy) or win_copy)
copy.prepare_input = util.concat
