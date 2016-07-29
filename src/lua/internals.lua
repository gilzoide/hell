--- @file internals.lua 
-- Functions to be used by hell only (suck that mango, user!)

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

local int = {}

int.cpp = require 'hell.cppUtils'

--- Hell's current version
int.version = '0.3.1'


--- Prints a message from hell execution
int.hellMsg = int.cpp.hellMsg
--- Prints a message from hell execution to stderr
int.hellErrMsg = int.cpp.hellErrMsg


--- Quits the program with a message, and sign possible error
function int.quit (msg, was_error)
	int.cpp.hellErrMsg (msg)
	os.exit (was_error and 1 or 0, true)
end


--- Assertion with custom quit handler (function quit)
--
-- @param cond The condition to be checked. If false, quit handler will
--  be called.
-- @param msg The message to be displayed. There's no default, please
--  provide one.
--
-- @return If condition is true, it's returned (just like assert does)
function int.assert_quit (cond, msg)
	if not cond then
		-- if called hell with the debug option, print the whole traceback
		if utils.getOption 'g' then
			msg = debug.traceback (msg)
		-- no hellfires yet, I know exactly where problem is
		elseif #int.path < 2 then
			local script = debug.getinfo (2)
			msg = table.concat {
				'[', script.short_src, ':', script.currentline, '] ', msg
			}
		else
			local script	-- function info
			local level = 1	-- call stack level
			local lastPath = int.path[#int.path]
			-- iterate through levels, until we find a script name
			-- that's not hell's internals
			repeat
				-- get function information
				script = debug.getinfo (level)
				if script.short_src:match (lastPath .. '/[^/]+') then
					::foundErrorSource::
					msg = table.concat {
						'[', script.short_src, ':', script.currentline, '] ', msg
					}
					break
				end
				level = level + 1
			until not script
		end
		int.quit (msg, true)
	end

	return cond
end


--- Iterator that returns only values from `rawget`
--
-- @param t The table
--
-- @return The iterator
function int.rawpairs (t)
	local key = nil
	local value

	return function ()
		repeat
			key, value = next (t, key)
			value = rawget (t, key)
		until key == nil or value ~= nil

		if key == nil then
			return nil
		else
			return key, value
		end
	end
end


--- A stack for the paths, which will be used when sourcing a hellfire,
-- for `build' and `install' to know where to look for inputs.
-- First path is where we are now
int.path = { int.cpp.getcwd () }

--- Get the path for the current hellbuild, from `from' to the end
--
-- @param from Trace path from which level?
-- 		0 : hell command initial working directory
-- 		Default = 1 : root hellbuild
-- 		2 : script's relative path to root build
-- @param to Which index to stop? Default: #int.path
--
-- @return The path trace, from `from' until the end
function int.getPath (from, to)
	from = from or 1
	to = to or #int.path

	local dir = table.concat (int.path, hell.os.dir_sep, from + 1, to)
	if dir ~= '' then
		dir = dir .. hell.os.dir_sep
	end

	return dir
end


--- Get the build path, it's important for the commands to be executed 
function int.getBuildPath (builder)
	-- initial buildPath = outdir, from builder or from the hell table
	local str = builder.outdir or hell.outdir

	if str then
		str =  str .. hell.os.dir_sep
	else
		str = ''
	end

	-- should we keep the build structure?
	if hell.keepDirStructure or builder.keepDirStructure then
		str = str .. int.getPath (2)
	end

	-- assert that the build directory exists, before we can use it
	-- like `mkdir -p`, create any intermediate directories
	local rootPath = int.getPath (0, 1):sub (1, -2)
	for dir in str:gmatch ('(.-)/') do
		rootPath = utils.makePath (rootPath, dir)
		int.cpp.createDirIfNeeded (rootPath)
	end

	-- update build path with script path
	if int.path[2] then
		str = int.cpp.lazyPrefix (int.path[2] .. hell.os.dir_sep, str)
	end

	return str
end

return int
