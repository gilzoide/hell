local util = hell.utils

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
	prepare_output = function (out, input)
		return out or util.changeExtension (input, hell.os.exe_ext)
	end,
	links = '',
	prepare_links = pkgconfig,
	cmd = '$bin $flags $input -o $output $links'
}

-- In C, we must first build the object files, then the executable, so do it!
function gcc.prepare_input (i, b)
	return util.fmap (i, function (ii)
		return pipeBuild (b, {
			flags = '&-c',
			input = ii,
			deps = {},
			prepare_input = util.id,
			prepare_output = function (_, input)
				return util.changeExtension (input, hell.os.obj_ext)
			end
		})
	end)
end

gcc.fpic = Builder {
	flags = '&-c -fPIC',
	prepare_input = util.id,
	prepare_output = function (_, input)
		return util.changeExtension (input, 'os')
	end
}

-- Shared libraries!
gcc.shared = Builder {
	flags = '&-shared',
	prepare_output = function (o, input)
		return util.changeExtension (input, hell.os.shared_ext)
	end
}

function gcc.shared.prepare_input (i, b)
	return util.fmap (i, function (ii)
		return pipeBuild (b, gcc.fpic:extend {
			input = ii
		})
	end)
end

-- default C builder (cuz it's the only one we have), so that the build function
-- can find it easily from the file's extension
c = gcc
