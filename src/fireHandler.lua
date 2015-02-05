--- @file fireHandler.lua
-- Hell script loading utilities

local int = require 'internals'


--- Function for sourcing a hell build script.
-- As a default, it uses the same _ENV as the previous one.
-- If you may want to scope the script (say, to modify some variable locally,
-- and then your changes are back to normal), just tell us!
--
-- @param script The script name, for loading
-- @param scope Should we scope _ENV? bool 
--
-- @return The results from loadfile
function int._addHellBuild (script, scope)
	local env = scope and setmetatable ({}, { __index = getfenv (1) }) or getfenv (1)
	local file, err = loadfile (script)
	if file then
		return setfenv (file, env)
	else
		return file, err
	end
end


--- Function for sourcing a hell build script.
-- Wrapper for the _addHellBuild function, which only hell should use
--
-- @param script The script name, for loading
-- @param scope Should we scope _ENV? bool 
--
-- @return The results from the script. Usualy none
function addHellBuild (script, scope)
	local file = int._addHellBuild (script, scope)

	int.assert_quit (file, "Can't load hellbuild \"" .. script .. '"', 2)
	int.hellMsg ('sourcing hellfire: ' .. script)
	
	local function takeDirectory (str)
		return str:match ("(.+)/.+") .. '/'
	end
	-- pushes path to internals.path, for knowing where we are
	table.insert (int.path, takeDirectory (script))

	local ret = file ()

	-- and pop the path, as we will go back now
	table.remove (int.path, #int.path)

	return ret
end
--- Alias for addHellBuild (I do like this one better xD)
feedHellFire = addHellBuild
