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

copy = Builder ((hell.os.name == 'unix' and unix_copy) or win_copy)
copy.prepare_input = function (input, b)
	return table.concat (util.fmap (input, function (i)
		return '"' .. i .. '"'
	end), ' ')
end
copy.prepare_output = function (out, b)
	local str = hell.outdir and b.input or '"copy_of_' .. b.input .. '"'
	return out or str
end
