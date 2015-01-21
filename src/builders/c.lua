local util = require 'hellutils'

-- Get the linking options for C compiling
-- If there ain't a pkg-config for it, just add the '-l' preffix
local function pkgconfig (links)
	return links:gsub ('%S+', function (pkg)
		return io.popen ('pkg-config --silence-errors --libs ' .. pkg):read () or '-l' .. pkg
	end)
end

gcc = Builder {
	bin = 'gcc',
	flags = '-Wall',
	prepare_input = util.concat,
	prepare_output = function (out, b)
		return out or b.input:gsub ('%.c', '')
	end,
	links = '',
	prepare_links = pkgconfig,
	cmd = '$bin $flags $input -o $output $links'
}

-- default C builder (cuz it's the only one we have), so that the build function
-- can find it easily from the file's extension
c = gcc
