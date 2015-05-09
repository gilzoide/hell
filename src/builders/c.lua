local util = hell.utils

-- Get the linking options for C compiling
-- If there ain't a pkg-config for it, just add the '-l' preffix
local function pkgconfig_link (links)
	return links:gsub ('%S+', function (pkg)
		return io.popen ('pkg-config --silence-errors --libs ' .. pkg):read () or '-l' .. pkg
	end)
end
local function pkgconfig_include_dirs (includes)
	return includes:gsub ('%S+', function (pkg)
		return io.popen ('pkg-config --silence-errors --cflags-only-I ' .. pkg):read () or '-I' .. pkg
	end)
end

gcc = Builder {
	bin = 'gcc',
	prepare_output = function (o, input)
		return o or util.changeExtension (input, hell.os.exe_ext)
	end,
	links = '',
	includes = '',
	prepare_links = pkgconfig_link,
	prepare_includes = pkgconfig_include_dirs,
	cmd = '$bin $flags $input -o $output $includes $links'
}

-- In C, we must first build the object files, then the executable, so do it!
function gcc.prepare_input (i, b)
	return util.fmap (i, function (ii)
		-- insert dependencies from `gcc -MM' on pipeBuild
		local deps = {}
		local gccMM = io.popen ('gcc -MM ' .. ii .. pkgconfig_include_dirs (b.includes or '') .. ' 2> /dev/null'):read () or ''
		for dependency in gccMM:gmatch ('%S%.%S') do
			table.insert (deps, dependency)
		end

		-- and now build the object file!
		return pipeBuild (b, {
			flags = '&-c',
			input = ii,
			deps = deps,
			prepare_input = false,
			prepare_output = function (_, input)
				return util.changeExtension (input, hell.os.obj_ext)
			end
		})
	end)
end

-- Shared libraries!
gcc.shared = Builder {
	flags = '-shared',
	prepare_output = function (o, input)
		return o or util.changeExtension (input, hell.os.shared_ext)
	end
}

function gcc.shared.prepare_input (i, b)
	return util.fmap (i, function (ii)
		return pipeBuild (b, {
			flags = '&-c -fPIC',
			input = ii,
			prepare_input = false,
			prepare_output = function (_, input)
				return util.changeExtension (input, hell.os.obj_ext)
			end
		})
	end)
end

-- default C builder (cuz it's the only one we have), so that the build function
-- can find it easily from the file's extension
c = gcc
