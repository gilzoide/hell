--- Handles builds and installs, should be used as _ENV's __newindex
local function build_install_Handler (t, k, v)
	if k == _G.hell.target then
		local meta = getmetatable (v)
		if meta == 'build' then
			table.insert (_G.hell.builds, v)
		elseif meta == 'install' then
			table.insert (_G.hell.installs, v)
		else
			rawset (t, k, v)
		end
	else
		rawset (t, k, v)
	end
end

--- Function for sourcing a hell build script.
--
-- As a default, it creates a new _ENV, for 'global' variables
-- being local to the script, so it doesn't affect the previous one. If you
-- like, you can pass your own _ENV so that it's shared between build scripts.
--
-- @param[in] script The script name, for loading
-- @param[in] env The script env. Default is creating a new one,
--  __indexing the old
--
-- @return The result from loadfile
function _addHellBuild (script, env)
	env = env or { __index = _ENV }
	-- for the hell builds to work, we need this __newindex
	env.__newindex = build_install_Handler
	setmetatable (env, env)
	if hell.verbose then
		print ('hell: sourcing hellbuild: ' .. script)
	end
	return loadfile (script, nil, env)
end
--- Alias for _addHellBuild (I do like this one better xD)
_feedHellFire = _addHellBuild


--- Wrapper for the _addHellBuild function, which quits the program if couldn't
-- load script.
--
-- It's the preferred approach for the final user, but less flexible.
--
-- @param[in] script The script name, for loading
-- @param[in] env The script env. Default is creating a new one,
--  __indexing the old
--
-- @return The results from the script. Usualy none
function addHellBuild (script, env)
	local file = assert (_addHellBuild (script, env))
	return file ()
end
--- Alias for addHellBuild (I do like this one better xD)
feedHellFire = addHellBuild