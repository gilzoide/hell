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

-- In C, we must first build the object files, then the executable, so do it!
function gcc.prepare_input (inputs, b)
	return table.concat (util.fmap (inputs, function (i)
		return pipeBuild (b, {
			flags = '&-c',
			input = i,
			deps = {},
			prepare_input = util.id,
			--function (inp, bb)
				--return util.prefixEach (inp, util.getBuildPath (bb))
			--end,
			prepare_output = function (_, bb)
				return util.changeExtension (bb.input, hell.os.obj_ext)
			end
		})
	end), ' ')
end

gcc.fpic = Builder {
	flags = '&-c -fPIC',
	prepare_input = util.id,
	prepare_output = function (_, b)
		return util.changeExtension (b.input, 'os')
	end
}

-- Shared libraries!
gcc.shared = Builder {
	flags = '&-shared',
	prepare_output = function (_, b)
		return util.changeExtension (b.input, hell.os.shared_ext)
	end
}

function gcc.shared.prepare_input (inputs, b)
	return table.concat (util.fmap (inputs, function (i)
		local obj = gcc.fpic {
			input = i
		}
		-- set `obj' as a dependency
		table.insert (b.deps, obj)
		return obj.output 
	end), ' ')
end

-- default C builder (cuz it's the only one we have), so that the build function
-- can find it easily from the file's extension
c = gcc
