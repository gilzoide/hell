--- @file hell.lua
-- The hell script executable

--[[
-- Copyright (C) 2015 Gil Barbosa Reis
-- This file is part of Hell.
--
-- Hell is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Hell is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Hell.  If not, see <http://www.gnu.org/licenses/>.
--]]

local int = require 'internals'
local util = require 'hellutils'

-- get OS. As linux/freebsd/solaris are all alike, we gather them as unix.
-- note that MacOSX is called "darwin" here (haskell puts it that way)
local OSes = {
	windows = {
		prefix = os.getenv ("APPDATA"),
		obj_ext = 'obj',
		shared_ext = 'dll',
		exe_ext = 'exe'
	},
	unix = {
		prefix = '/usr',
		obj_ext = 'o',
		shared_ext = 'so',
		exe_ext = ''
	},
	darwin = {
		prefix = '/usr',
		obj_ext = 'o',
		shared_ext = 'dynlib',
		exe_ext = ''
	}
}

local os = OSes[int.cpp.getOS ()] or OSes.unix
os.name = int.cpp.getOS ()
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
	os = os,
	-- let users use the utils!
	utils = util
}

local opts = (assert (loadfile ('parseOpts.lua'))) (...)

--[[		And now, source our first hellbuild script.
	It looks respectively into 'opts.file', './hellfire', './hellbuild'		]]--
local BI = require 'build_install'
require 'Builder'
require 'fireHandler'

-- first script to load
local script, err, env
local build_scripts = { opts.f }
table.insert (build_scripts, './hellfire')
table.insert (build_scripts, './hellbuild')

int.hellMsg ('reading build script(s)')
for i = #build_scripts, 1, -1 do
	script, err = int._addHellBuild (build_scripts[i], true, 1)
	if not script then
		if not err:match ('open') then
			int.quit ("lua: " .. err, true)
		end
	else
		env = err
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

int.hellMsg ("all set, let's see what we got\n")

-- Called for help?
if opts.h then
	hellp ()
end

opts.target = opts.target or ''
local target = int.assert_quit (util.getNestedField (env, opts.target),
		"Can't find target \"" .. opts.target .. '"')

-- maybe get available targets?
if opts.l then
	int.hellMsg ("Listing available targets:")
	BI.listTargets (env)
	return
end


-- Command to be executed (build | clean | install | uninstall)
if opts.command == 'build' or opts.command == 'clean' then
	if opts.target ~= '' then
		BI.builds = BI.getBI (target, 'build')
	end
	int.assert_quit (#BI.builds ~= 0, "Can't find any builds" .. (opts.target and ' in target "' .. opts.target .. '"' or ''))

	-- let's clean stuff instead of building
	if opts.command == 'clean' then
		BI.builds = BI.makeClean (BI.builds)
	end

	-- Process the builds in Haskell
	int.cpp.processBI (BI.builds)
else -- opts.command == 'install' or opts.command == 'uninstall'
	if opts.target ~= '' then
		BI.installs = BI.getBI (target, 'install')
	end
	int.assert_quit (#BI.installs ~= 0, "Can't find any installs" .. (opts.target and ' in target "' .. opts.target .. '"' or ''))

	-- Process the installs in Haskell
	int.cpp.processBI (BI.installs)
end
