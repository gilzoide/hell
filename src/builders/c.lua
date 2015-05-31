local util = hell.utils

-- Get the linking options for C compiling
-- If there ain't a pkg-config for it, just add the '-l' preffix
local function pkgconfig_link (links)
	return links and util.fmap (links, function (pkg)
			return pkg and (util.shell ('pkg-config --silence-errors --libs ' .. pkg) or '-l' .. util.makeRelative (pkg, util.getcwd () .. hell.os.dir_sep))
		end)
end
local function pkgconfig_include_dirs (includes)
	return includes and util.fmap (includes, function (pkg)
			return util.shell ('pkg-config --silence-errors --cflags-only-I ' .. pkg) or '-I' .. util.makeRelative (pkg, util.getcwd () .. hell.os.dir_sep)
		end) or ''
end

gcc = Builder {
	bin = 'gcc',
	prepare_output = function (o, input)
		return o or util.changeExtension (input, hell.os.exe_ext)
	end,
	links = nil,
	flags = '-Wall',
	includes = nil,
	prepare_links = pkgconfig_link,
	prepare_includes = pkgconfig_include_dirs,
	cmd = '$bin -o $output $input $flags $includes $links',
	help = "Compiles a C program, pipeBuilding all of the input files as objects first"
}

-- In C, we must first build the object files, then the executable, so do it!
function gcc.prepare_input (i, b)
	return util.fmap (i, function (ii)
		-- insert dependencies from `gcc -MM' on pipeBuild
		--local deps = {}
		--local gccMM = util.shell ('gcc -MM ' .. ii .. util.concat (pkgconfig_include_dirs (b.includes or ''))) or ''
		--for dependency in gccMM:gmatch ('%S%.%S') do
			--print (dependency)
			--table.insert (deps, dependency)
		--end

		-- and now build the object file!
		return pipeBuild (b, {
			flags = '&-c',
			input = ii,
			deps = deps,
			links = '',
			prepare_links = false,
			prepare_input = false,
			prepare_output = function (_, input)
				return util.changeExtension (input, hell.os.obj_ext)
			end
		})
	end)
end

-- Shared libraries!
gcc.shared = Builder {
	prepare_flags = function (f) return '-shared ' .. (f or '') end,
	prepare_output = function (o, input)
		return o or util.changeExtension (input, hell.os.shared_ext)
	end,
	help = "Compiles a C shared library, pipeBuilding all of the input files as PIC objects first"
}

function gcc.shared.prepare_input (i, b)
	return util.fmap (i, function (ii)
		return pipeBuild (b, {
			flags = '&-c -fPIC',
			input = ii,
			prepare_input = false,
			prepare_flags = util.id,
			prepare_output = function (_, input)
				return util.changeExtension (input, hell.os.obj_ext)
			end
		})
	end)
end

-- default C builder (cuz it's the only one we have), so that the build function
-- can find it easily from the file's extension
c = gcc

-- Auxiliary function: checks package in `pkg-config`
function util.checkPkgConfig (pkg)
	return util.shell ('pkg-config --list-all | grep ' .. pkg) and pkg
end
