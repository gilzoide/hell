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
	prepare_output = function (out, b)
		return out or util.changeExtension (util.concat (b.input), hell.os.exe_ext)
	end,
	links = '',
	prepare_links = pkgconfig,
	cmd = '$bin $flags $input -o $output $links'
}

function gcc.prepare_input (ins, b)
	return util.fmap (ins, function (i)
		local obj = build (b:extend {
			flags = '&-c',
			input = i,
			prepare_input = util.id,
			prepare_output = function (_, bb)
				return util.changeExtension (bb.input, hell.os.obj_ext)
			end
		})
		return obj.output
	end)
end

-- default C builder (cuz it's the only one we have), so that the build function
-- can find it easily from the file's extension
c = gcc
