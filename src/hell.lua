--- @file hell.lua
-- The hell script executable

--- Quits the program with a message, and sign possible error
function quit (msg, was_error)
	io.stderr:write ('hell: ' .. msg .. '\n')
	os.exit (was_error and 0 or 1, true)
end

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
	-- the custom help message. If nil/false, use default help string
	help = nil,
	-- the target. If nil/false, build/install everything!
	target = nil,
	-- output directory: used for 'shadow builds'
	-- if nil/false, do the builds in the build scripts' own path
	-- note that if `outdir = '.'`, hell uses the root script's path
	outdir = nil,
	-- keep the directory structure when building?
	-- doesn't make sense when not shadow building (when outdir not especified)
	-- Ex: if input is at './src', output goes to the '$outdir/src' dir
	keepDirStructure = nil,
	-- table with all the builds. Please don't mess with it xP
	builds = {},
	-- table with all the installs. Please don't mess with it xP
	installs = {},
	-- ao conferir arquivos de entrada com o glob, usar as entradas em 
	-- um comando só?
	multinput = true,
	-- table with some SO especific stuff
	os = package.config:sub (1, 1) == '/' and unix or win,
}

local opts = (assert (loadfile ('parseOpts.lua'))) (...)

--[[		And now, source our first hellbuild script.
	It looks respectively into 'opts.file', './hellfire', './hellbuild'		]]--
require 'Builder'
require 'fireHandler'

-- file specified
if opts.f then 
	addHellBuild (opts.f)
-- or the default ones
else
	local script, err = _addHellBuild ('./hellfire')
	if not script then
		if not err:match ('open') then
			quit ('lua: ' .. err, true)
		else
			script, err = _addHellBuild ('./hellbuild')
			if not script and err:match ('open') then
				quit ("Can't find 'hellbuild' or 'hellfire' build scripts", true)
			else
				quit ('lua: ' .. err, true)
			end
		end
	end
	script ()
end

-- Called for help?
if opts.h or opts.H then
	hellp ()
end

-- Process the builds
if #hell.builds == 0 then
	quit ("Can't find any builds" .. (hell.target and ' in target "' .. hell.target .. '"' or ''))
end

for k, v in ipairs (hell.builds) do
	print (v.cmd)
end
