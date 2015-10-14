-- Get the linking options for C compiling
-- If there ain't a pkg-config for it, just add the '-l' preffix
local function pkgconfig_link (links)
	return links and utils.fmap (function (pkg)
			return utils.shell ('pkg-config --silence-errors --libs-only-l ' .. pkg) or '-l' .. pkg
		end, links) or ''
end
local function pkgconfig_lib_dirs (libDirs)
	return libDirs and utils.fmap (function (pkg)
			return utils.shell ('pkg-config --silence-errors --libs-only-L ' .. pkg) or '-L' .. utils.makeRelative (utils.getcwd () .. hell.os.dir_sep, pkg)
		end, libDirs) or ''
end
local function pkgconfig_include_dirs (includes)
	return includes and utils.fmap (function (pkg)
			return utils.shell ('pkg-config --silence-errors --cflags-only-I ' .. pkg) or '-I' .. utils.makeRelative (utils.getcwd () .. hell.os.dir_sep, pkg)
		end, includes) or ''
end
local function prepare_std (std)
	return std and '-std=' .. std or ''
end

gcc = Builder {
	bin = 'gcc',
	prepare_output = function (o, input)
		return o or utils.changeExtension (hell.os.exe_ext, input)
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
	help = [[Compiles a C program, pipeBuilding all of the input files as objects first

By default, it checks for dependencies with `gcc -MM`, which may be too slow if
your project is too big. For those cases, specify field 'skipDepCheck' as true

Fields:
=======
bin - Compiler command. Default = "gcc"
output - The output name, suffixed with '.exe' if necessary
flags - The compilation general flags. Default = "-Wall"
std - The standard to be used
includes - Header path, found with `pkg-config --cflags-only-I` or prefixed with '-I'
links - Shared libraries links, found with `pkg-config --libs-only-l` or prefixed with '-l'
libDirs - Linker path, found with `pkg-config --libs-only-L` or prefixed with '-L'
skipDepCheck - Should we skip dependency check with `gcc -MM`?
]]
}


local function getGccMMDeps (input, builder)
	-- insert dependencies from `gcc -MM' on pipeBuild
	local gccDeps = {}
	local gccMM = utils.shell ('gcc -MM ' .. input .. ' ' .. utils.concat (pkgconfig_include_dirs (builder.includes or '')) .. ' ' .. prepare_std (builder.std)) or ''
	-- ignore "target:"
	gccMM = gccMM:match ('.*: (.*)') or ''
	--print ('gccMM', gccMM)
	for dependency in gccMM:gmatch ('%S+[^\\%s]') do
		-- if relative path, add root hellbuild path
		if dependency:sub (1, 1) ~= '/' then
			dependency = utils.makeRelative (utils.getPath (), dependency)
		end
		table.insert (gccDeps, dependency)
	end

	return gccDeps
end


-- In C, we must first build the object files, then the executable, so do it!
function gcc.prepare_input (i, b)
	return utils.fmap (function (ii)
		deps = b.skipDepCheck and {} or getGccMMDeps (ii, b)
		-- and now build the object file!
		return pipeBuild (b, {
			flags = '&-c',
			input = ii,
			deps = deps,
			links = '',
			libDirs = '',
			prepare_links = false,
			prepare_libDirs = false,
			prepare_input = false,
			prepare_output = function (_, input)
				return utils.changeExtension (hell.os.obj_ext, input)
			end
		})
	end, i)
end

-- Shared libraries!
gcc.shared = Builder {
	prepare_flags = function (f) return '-shared ' .. (f or '') end,
	prepare_output = function (o, input)
		return o or utils.changeExtension (hell.os.shared_ext, input)
	end,
	help = "Compiles a C shared library, pipeBuilding all of the input files as PIC objects first"
}

function gcc.shared.prepare_input (i, b)
	return utils.fmap (function (ii)
		return pipeBuild (b, {
			flags = '&-c' .. (hell.os.name == 'windows' and '' or ' -fPIC'),
			input = ii,
			deps = getGccMMDeps (ii, b),
			prepare_input = false,
			prepare_flags = false,
			prepare_output = function (_, input)
				return utils.changeExtension (hell.os.obj_ext, input)
			end
		})
	end, i)
end

-- default C builder (cuz it's the only one we have yet), so that the build
-- function can find it easily from the file's extension
c = gcc

-- Auxiliary function: checks package in `pkg-config`
function utils.checkPkgConfig (pkg)
	return utils.shell ('pkg-config --list-all | grep ' .. pkg) and pkg
end
