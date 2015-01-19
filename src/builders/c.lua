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
	links = '',
	prepare_links = pkgconfig,
	cmd = '$bin $flags $input -o $output $links'
}
