local util = require 'hellutils'

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
copy.multinput = true
copy.prepare_output = function (out, b)
	local str = (hell.outdir and '' or 'copy_of_') .. b.input
	return out or str
end
