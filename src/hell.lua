--- @file hell.lua
-- The hell script executable

--- Quits the program with a message, and sign possible error
function quit (msg, was_error)
	io.stderr:write ('hell: ' .. msg .. '\n')
	os.exit (was_error and 0 or 1, true)
end

--- Assertion with custom quit handler (function quit)
--
-- @param[in] cond The condition to be checked. If false, quit handler will
--  be called.
-- @param[in] msg The message to be displayed. There's no default, please
--  provide one.
-- @param[in] level Debug level, for showing where the problem happend.
--  Set the level just like you would in debug.getinfo, as assert_quit already
--  increments itself in the level.
--
-- @return If condition is true, it's returned (just like assert does)
function assert_quit (cond, msg, level)
	if not cond then
		-- maybe we want to trace where the problem happened, so...
		if level then
			-- need to set level+1, so we don't count assert_quit itself
			local script = debug.getinfo (level + 1)
			msg = script.short_src .. ':' .. script.currentline .. ': ' .. msg
		end
		quit (msg, true)
	else
		return cond
	end
end

--- Prints a message from hell execution
function hellMsg (msg)
	if hell.verbose () ~= false then
		print ('hell: ' .. msg)
	end
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

-- Command to be executed (build | clean | install | uninstall)
if opts.command == 'build' then
	-- Process the builds
	assert_quit (#hell.builds ~= 0, "Can't find any builds" .. (hell.target and ' in target "' .. hell.target .. '"' or ''))

	for k, v in ipairs (hell.builds) do
		print ((not hell.verbose () and v.echo) or v.cmd)
	end
elseif opts.command == 'clean' then
-- clean
elseif opts.command == 'install' then
	-- Process the installs
	assert_quit (#hell.installs ~= 0, "Can't find any installs" .. (hell.target and ' in target "' .. hell.target .. '"' or ''))

	for k, v in ipairs (hell.installs) do
		print ((not hell.verbose () and v.echo) or v.cmd)
	end
else -- opts.command == 'uninstall'

end
