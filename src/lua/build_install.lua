--- @file build_install.lua
-- build and install functions, vital pieces of the hell build system

local util = require 'hellutils'
local int = require 'internals'

local BI = {}

BI.builds = {}
BI.installs = {}

--- Auxiliary function for getting the builds/installs from a table
function BI.getBI (t, meta)
	if type (t) ~= 'table' then
		return nil
	end

	local ret = {}
	for i, v in ipairs (t) do
		if getmetatable (v) == meta then
			table.insert (ret, v)
		end
	end

	return ret
end


--- List all targets from table `t'
--
-- @note ListTargets uses rawpairs, so that we don't recurse over tables we
-- know won't have any builds/installs (as lua libs)
--
-- @param The table (should be called with the first script environment)
function BI.listTargets (t)
	local ret = {}
	local acum = {}

	--- Get all available targets in table `t'
	--
	-- @param acum Table acumulating the current table name
	--  (with dot notation "outer.inner"), so we can write it nicely
	-- @param t The value being checked (probably a table)
	-- @param current Name of the current value being checked, so that we don't
	--  always need to be pushing and popping from `acum'
	local function listTargets (t, current)
		--print ('  listing: ' .. current)
		if type (t) ~= 'table' then
			return nil
		elseif getmetatable (t) == 'build' or getmetatable (t) == 'install' then
			return true
		end

		local thisIsATarget = false

		table.insert (acum, current)
		-- look at every entry in `t': if any have a "build"/"install" metatable
		-- inside, mark this as target
		for k, v in int.rawpairs (t) do
			if listTargets (v, k) then
				thisIsATarget = true
			end
		end

		if thisIsATarget and current then
			table.insert (ret, table.concat (acum, '.'))
		end
		table.remove (acum)
	end

	listTargets (t)
	-- print targets in reverse, as the recursion fetches them in this order
	for i = #ret, 1, -1 do
		print (ret[i])
	end
end


--- Gets the default builder, based on input's extension
local function getDefaultBuilder (builder)
	local auto_builder
	if not builder.builder then
		local ext = builder.input:match ('.-%.(%S+)') 
		auto_builder = getfenv(1)[ext] or copy
	else
		int.assert_quit (getmetatable (builder.builder) == 'hellbuilder',
				"Trying to use an invalid Builder", 2)
		-- calling build with explicit builder field
		auto_builder = builder.builder
	end
	return auto_builder:extend (builder)
end


--- get the build path, it's important for the commands to be executed 
local function getBuildPath (builder)
	local str = ''
	if hell.outdir then
		str = hell.outdir .. hell.os.dir_sep
	end

	if hell.keepDirStructure or builder.keepDirStructure then
		str = str .. int.getPath (2)
	end

	return str
end


--- The build function, for building anything!
function build (builder)
	-- if called build function explicitly, search for the builder
	-- defaults to it's extension, or fallback to copy
	if getmetatable (builder) ~= 'hellbuilder' then
		builder = getDefaultBuilder (builder)
	end
	-- we need a command, man
	int.assert_quit (type (builder.cmd) == 'string',
			"Can't build something without a command.\
Needed a string, got a " .. type (builder.input) .. '.', 2)
	-- the new build
	local new_cmd = util.substField (builder, 'cmd')
	local new = {
		__metatable = 'build',
		echo = builder.echo,
		input = builder.input,
		output = builder.output or builder.input,
		deps = builder.deps,
		cmd = util.substField (builder:extend {
			prepare_input = util.curryPrefixEach (int.getPath ()),
			prepare_output = function (out, b)
				return getBuildPath (b) .. out
			end
		}, 'cmd')
	}
	setmetatable (new, new)

	table.insert (_G, new)
	return new
end

--- The install function
function install (builder)
	-- for install, always copy
	local new_cmd = util.substField (copy:extend {
		input = builder.output or builder.input 
	}, 'cmd')
	local new = {
		__metatable = 'install',
		echo = builder.echo,
		input = builder.input,
		output = builder.output,
		cmd = new_cmd
	}
	setmetatable (new, new)

	table.insert (_G, new)
	return new
end

return BI
