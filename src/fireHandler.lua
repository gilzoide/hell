--- Function for sourcing a hell build script.
--
-- As a default, it uses the same _ENV as the previous one.
-- If you may want to scope the script (say, to modify some variable locally,
-- and then your changes are back to normal), just tell us!
--
-- @param script The script name, for loading
-- @param scope Should we scope _ENV? bool
--
-- @return The result from loadfile
function _addHellBuild (script, scope)
	local env = scope and setmetatable ({}, { __index = _ENV }) or _ENV
	return loadfile (script, nil, env)
end
--- Alias for _addHellBuild (I do like this one better xD)
_feedHellFire = _addHellBuild


--- Wrapper for the _addHellBuild function, which quits the program if couldn't
-- load script.
--
-- It's the preferred approach for the final user, but less flexible.
--
-- @param script The script name, for loading
-- @param scope Should we scope _ENV? bool 
--
-- @sa _addHellBuild
--
-- @return The results from the script. Usualy none
function addHellBuild (script, scope)
	local file = _addHellBuild (script, scope)
	assert_quit (file, "Can't load hellbuild \"" .. script .. '"', 2)
	hellMsg ('sourcing hellbuild: ' .. script)
	return file ()
end
--- Alias for addHellBuild (I do like this one better xD)
feedHellFire = addHellBuild
