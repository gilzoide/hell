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
-- @return The script loaded, and it's env (which may be canged) if file loaded
-- @return Nil and error message, if didn't load well
function int._addHellBuild (script, scope)
	level = level or 3
	local env = scope and setmetatable ({}, { __index = _ENV }) or _ENV

	local file, err = loadfile (int.getPath () .. script, nil, env)
	if file then
		-- pushes path to internals.path, for knowing where we are
		local function takeDirectory (path)
			 return path:match ("(.+)/.+")
		end
		table.insert (int.path, takeDirectory (script))

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

	int.assert_quit (file, "Can't load hellbuild \"" .. script .. '"', 2)
	int.hellMsg ('sourcing hellfire: ' .. script)
	
	local ret = file ()

	-- and pop the path, as we will go back now
	table.remove (int.path)

	return ret, env
end
--- Alias for addHellBuild (I do like this one better xD)
feedHellFire = addHellBuild
