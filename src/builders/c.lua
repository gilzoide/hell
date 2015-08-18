local util = hell.utils

-- Get the linking options for C compiling
-- If there ain't a pkg-config for it, just add the '-l' preffix
local function pkgconfig_link (links)
	return links and util.fmap (links, function (pkg)
			return util.shell ('pkg-config --silence-errors --libs-only-l ' .. pkg) or '-l' .. util.makeRelative (pkg, util.getcwd () .. hell.os.dir_sep)
		end) or ''
end
local function pkgconfig_lib_dirs (libDirs)
	return libDirs and util.fmap (libDirs, function (pkg)
			return util.shell ('pkg-config --silence-errors --libs-only-L ' .. pkg) or '-L' .. util.makeRelative (pkg, util.getcwd () .. hell.os.dir_sep)
		end) or ''
end
local function pkgconfig_include_dirs (includes)
	return includes and util.fmap (includes, function (pkg)
			return util.shell ('pkg-config --silence-errors --cflags-only-I ' .. pkg) or '-I' .. util.makeRelative (pkg, util.getcwd () .. hell.os.dir_sep)
		end) or ''
end
local function prepare_std (std)
	return std and '-std=' .. std or ''
end

gcc = Builder {
	bin = 'gcc',
	prepare_output = function (o, input)
		return o or util.changeExtension (input, hell.os.exe_ext)
	end,
	links = nil,
	flags = '-Wall',
	includes = nil,
	libDirs = nil,
	std = nil,
	skipDepCheck = nil,
	prepare_links = pkgconfig_link,
	prepare_libDirs = pkgconfig_lib_dirs,
	prepare_includes = pkgconfig_include_dirs,
	prepare_std = prepare_std,
	cmd = '$bin -o $output $input $std $flags $includes $libDirs $links',
	help = [[Compiles a C program, pipeBuilding all of the input files as objects
first
By default, it checks for dependencies with `gcc -MM`, which may be too slow if
your project is too big. For those cases, specify field 'skipDepCheck' as true]]
}


local function getGccMMDeps (input, builder)
	-- insert dependencies from `gcc -MM' on pipeBuild
	local gccDeps = {}
	local gccMM = util.shell ('gcc -MM ' .. input .. ' ' .. util.concat (pkgconfig_include_dirs (builder.includes or '')) .. ' ' .. prepare_std (builder.std)) or ''
	-- ignore "target:"
	gccMM = gccMM:match ('.*: (.*)') or ''
	--print ('gccMM', gccMM)
	for dependency in gccMM:gmatch ('%S+[^\\%s]') do
		-- if relative path, add root hellbuild path
		if dependency:sub (1, 1) ~= '/' then
			dependency = util.makeRelative (dependency, util.getPath ())
		end
		table.insert (gccDeps, dependency)
	end

	return gccDeps
end


-- In C, we must first build the object files, then the executable, so do it!
function gcc.prepare_input (i, b)
	return util.fmap (i, function (ii)
		deps = b.skipDepCheck and {} or getGccMMDeps (ii, b)
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
			deps = getGccMMDeps (ii, b),
			prepare_input = false,
			prepare_flags = false,
			prepare_output = function (_, input)
				return util.changeExtension (input, hell.os.obj_ext)
			end
		})
	end)
end

-- default C builder (cuz it's the only one we have yet), so that the build
-- function can find it easily from the file's extension
c = gcc

-- Auxiliary function: checks package in `pkg-config`
function util.checkPkgConfig (pkg)
	return util.shell ('pkg-config --list-all | grep ' .. pkg) and pkg
end
