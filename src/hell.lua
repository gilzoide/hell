--- @file hell.lua
-- The hell script executable

--- Quits the program with a message, and sign possible error
function quit (msg, was_error)
	io.stderr:write ('hell: ' .. msg .. '\n')
	os.exit (was_error and 0 or 1, true)
end

--- Assertion with custom quit handler (function quit)
--
-- @param cond The condition to be checked. If false, quit handler will
--  be called.
-- @param msg The message to be displayed. There's no default, please
--  provide one.
-- @param level Debug level, for showing where the problem happend.
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
	end

	return cond
end

--[[		hell: the table that controls everything that's going on		]]--
local win = {
	name = 'windows',
	architecture = os.getenv ('PROCESSOR_ARCHITECTURE'),
	dir_sep = '\\',
	obj_ext = 'obj',
	exe_ext = 'exe'
}

local unix = {
	name = 'unix',
	architecture = io.popen ('uname -m'):read (),
	dir_sep = '/',
	obj_ext = 'o',
	exe_ext = ''
}

hell = {
	-- the custom help message. If nil/false, use default help string
	help = nil,
	-- output directory: used for 'shadow builds'
	-- if nil/false, do the builds in the build scripts' own path
	-- note that if `outdir = '.'`, hell uses the root script's path
	outdir = nil,
	-- keep the directory structure when building?
	-- doesn't make sense when not shadow building (when outdir not especified)
	-- Ex: if input is at './src', output goes to the '$outdir/src' dir
	keepDirStructure = nil,
	-- ao conferir arquivos de entrada com o glob, usar as entradas em 
	-- um comando só?
	multinput = true,
	-- table with some SO especific stuff
	os = package.config:sub (1, 1) == '/' and unix or win,
}

local opts = (assert (loadfile ('parseOpts.lua'))) (...)

--- Prints a message from hell execution
function hellMsg (msg)
	if opts.verbose ~= false then
		print ('hell: ' .. msg)
	end
end

--[[		And now, source our first hellbuild script.
	It looks respectively into 'opts.file', './hellfire', './hellbuild'		]]--
local BI = require 'build_install'
local util = require 'hellutils'
require 'Builder'
require 'fireHandler'

-- file specified
local script, err
local build_scripts = {opts.f}
table.insert (build_scripts, './hellfire')
table.insert (build_scripts, './hellbuild')

for i = #build_scripts, 1, -1 do
	script, err = _addHellBuild (build_scripts[i])
	if not script then
		if not err:match ('open') then
			quit ("lua: " .. err, true)
		end
	else
		break
	end
end

-- well, let's say the user asked for help but there's no hellbuild to source
-- no need to let him down, right?
if not script and opts.h then
	hellp ()
end

assert_quit (script, "Can't find any build scripts. Tried \"" .. table.concat (build_scripts, '", "') .. '"')
script ()

-- Called for help?
if opts.h then
	hellp ()
elseif opts.l then

end

opts.target = opts.target or '_G'
local target = assert_quit (util.getNestedField (_G, opts.target),
		"Can't find target \"" .. opts.target .. '"')
-- Command to be executed (build | clean | install | uninstall)
if opts.command == 'build' or opts.command == 'clean' then
	if opts.target then
		BI.builds = BI.getBI (target, 'build')
	end
	-- Process the builds
	assert_quit (#BI.builds ~= 0, "Can't find any builds" .. (opts.target and ' in target "' .. opts.target .. '"' or ''))

	for k, v in ipairs (BI.builds) do
		print ((not opts.verbose and v.echo) or v.cmd)
	end
else -- opts.command == 'install' or opts.command == 'uninstall'
	if opts.target then
		BI.installs = BI.getBI (target, 'install')
	end
	-- Process the installs
	assert_quit (#BI.installs ~= 0, "Can't find any installs" .. (opts.target and ' in target "' .. opts.target .. '"' or ''))

	for k, v in ipairs (BI.installs) do
		print ((not opts.verbose and v.echo) or v.cmd)
	end
end
