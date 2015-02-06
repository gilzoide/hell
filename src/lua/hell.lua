--- @file hell.lua
-- The hell script executable


-- get OS. As linux/freebsd/solaris are all alike, we gather them as unix.
-- note that MacOSX is called "darwin" here (haskell puts it that way)
local OSes = {
	windows = {
		obj_ext = 'obj',
		shared_ext = 'dll',
		exe_ext = 'exe'
	},
	unix = {
		obj_ext = 'o',
		shared_ext = 'so',
		exe_ext = ''
	},
	darwin = {
		obj_ext = 'o',
		shared_ext = 'dynlib',
		exe_ext = ''
	}
}

local os = OSes[getOS ()] or OSes.unix
os.name = getOS ()
os.dir_sep = package.config:sub (1, 1)

--[[		hell: the table that controls everything that's going on		]]--
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
	-- table with some SO especific stuff
	os = os
}

local opts = (assert (loadfile ('parseOpts.lua'))) (...)

--[[		And now, source our first hellbuild script.
	It looks respectively into 'opts.file', './hellfire', './hellbuild'		]]--
local BI = require 'build_install'
local util = require 'hellutils'
local int = require 'internals'
require 'Builder'
require 'fireHandler'

-- first script to load
local script, err
local build_scripts = {opts.f}
table.insert (build_scripts, './hellfire')
table.insert (build_scripts, './hellbuild')

int.hellMsg ('reading build script(s)')
for i = #build_scripts, 1, -1 do
	script, err = int._addHellBuild (build_scripts[i])
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

-- process root build script
int.assert_quit (script, "Can't find any build scripts. Tried \"" .. table.concat (build_scripts, '", "') .. '"')
script ()

int.hellMsg ("all set, let's see what we got")

-- Called for help?
if opts.h then
	hellp ()
elseif opts.l then

end


opts.target = opts.target or '_G'
local target = int.assert_quit (util.getNestedField (_G, opts.target),
		"Can't find target \"" .. opts.target .. '"')
-- Command to be executed (build | clean | install | uninstall)
if opts.command == 'build' or opts.command == 'clean' then
	if opts.target then
		BI.builds = BI.getBI (target, 'build')
	end
	-- Process the builds
	int.assert_quit (#BI.builds ~= 0, "Can't find any builds" .. (opts.target and ' in target "' .. opts.target .. '"' or ''))

	for k, v in ipairs (BI.builds) do
		print ((not int.verbose and v.echo) or v.cmd)
		for _, dep in ipairs (v.deps or {}) do
			print ('\t' .. ((not int.verbose and dep.echo) or dep.cmd))
		end
	end
else -- opts.command == 'install' or opts.command == 'uninstall'
	if opts.target then
		BI.installs = BI.getBI (target, 'install')
	end
	-- Process the installs
	int.assert_quit (#BI.installs ~= 0, "Can't find any installs" .. (opts.target and ' in target "' .. opts.target .. '"' or ''))

	for k, v in ipairs (BI.installs) do
		print ((not int.verbose and v.echo) or v.cmd)
	end
end
