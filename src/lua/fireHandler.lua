--- @file fireHandler.lua
-- Hell script loading utilities

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


--- Function for sourcing a hell build script.
-- As a default, it uses the same _ENV as the previous one.
-- If you may want to scope the script (say, to modify some variable locally,
-- and then your changes are back to normal), just tell us!
--
-- @param script The script name, for loading
-- @param scope Should we scope _ENV? bool
--
-- @return The script loaded, and it's env (which may be canged) if file loaded
-- @return Nil and error message, if didn't load well
function int._addHellBuild (script, scope)
	level = level or 3
	local env = scope and setmetatable ({}, { __index = _ENV }) or _ENV

	local file, err = loadfile (script, nil, env)
	if file then
		-- pushes path to internals.path, for knowing where we are
		table.insert (int.path, utils.takeDirectory (script))
		int.cpp.chdir (int.getPath (0))

		return file, env
	else
		return nil, err
	end
end


--- Function for sourcing a hell build script.
-- Wrapper for the _addHellBuild function, which only hell should use
--
-- @param script The script name, for loading
-- @param scope Should we scope _ENV? bool 
--
-- @return The results from the script, and the environment
function addHellBuild (script, scope)
	local file, env = int._addHellBuild (script, scope)

	int.assert_quit (file, "Can't load hellbuild \"" .. script .. '"')
	int.hellMsg (string.rep ('  ', #int.path - 2) .. 'sourcing hellfire: ' .. script)
	
	local ret = file ()

	-- and pop the path, as we will go back now
	table.remove (int.path)
	int.cpp.chdir (int.getPath (0))

	return ret, env
end
--- Alias for addHellBuild (I do like this one better xD)
feedHellFire = addHellBuild
