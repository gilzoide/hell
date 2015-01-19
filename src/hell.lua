--- @file hell.lua
-- The hell script executable

--- Quits the program with a message, and sign possible error
function quit (msg, was_error)
	io.stderr:write ('hell: ' .. msg .. '\n')
	os.exit (was_error and 0 or 1, true)
end

--- Function for sourcing a hell build script.
--
-- As a default, it creates a new _ENV, for 'global' variables
-- being local to the script, so it doesn't affect the previous one. If you
-- like, you can pass your own _ENV so that it's shared between build scripts.
--
-- @param[in] script The script name, for loading
-- @param[in] env The script env. Default is creating a new one,
--  __indexing the old
--
-- @return The results from the script. Usualy none
function addHellBuild (script, env)
	env = env or { __index = _ENV }
	setmetatable (env, env)
	local file = loadfile (script, nil, env)
	if not file then quit ("Can't find file \"" .. script .. "\"", true) end
	return file ()
end
--- Alias for addHellBuild (I do like this one more xD)
feedHellFire = addHellBuild

--[[		hell: the table that controls everything that's going on		]]--
local win = {
	name = 'windows',
	architecture = os.getenv ('PROCESSOR_ARCHITECTURE'),
	dir_sep = '\\'
}

local unix = {
	name = 'unix',
	architecture = io.popen ('uname -m'):read (),
	dir_sep = '/'
}

hell = {
	-- the custom help message. If false, use default help string
	help = false,
	-- output directory: used for 'shadow builds'
	-- if '', do the builds in the build scripts' own path
	-- note that if `outdir = '.'`, hell uses the root script's path
	outdir = '',
	-- keep the directory structure when building?
	-- doesn't make sense when not shadow building (when outdir not especified)
	-- Ex: if input is at './src', output goes to the '$outdir/src' dir
	keepDirStructure = false,
	-- table with all the builds. Please don't mess with it xP
	builds = {},
	-- table with all the installs. Please don't mess with it xP
	installs = {},
	-- ao conferir arquivos de entrada com o glob, usar as entradas em 
	-- um comando só?
	multinput = true,
	-- table with some SO especific stuff
	os = package.config:sub (1, 1) == '/' and unix or win,
	-- don't let people mess too much in the hell table
	__newindex = function ()
		error ("If you praise for your life, don't mess with HELL (the Table)!")
	end
}

-- for __newindex to work (metamethods work only on metatables)
setmetatable (hell, hell)

local opts = (loadfile ('parseOpts.lua')) (...)

--[[		And now, source our first hellbuild script.
	It looks respectively into 'opts.file', './hellfire', './hellbuild'		]]--
require 'Builder'

if opts.f then 
	addHellBuild (opts.f)
else
	local script = loadfile ('./hellfire') or loadfile ('./hellbuild')
	if not script then quit ("Can't find 'hellbuild' or 'hellfire' build scripts", true) end
	script ()
end
