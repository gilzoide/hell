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


local function getDefaultBuilder (builder)
	local auto_builder
	if not builder.builder then
		local ext = builder.input:match ('.-%.(%S+)') 
		auto_builder = _ENV[ext] or copy
	else
		int.assert_quit (getmetatable (builder.builder) == 'hellbuilder',
				"Trying to use an invalid Builder", 2)
		-- calling build with explicit builder field
		auto_builder = builder.builder
	end
	return auto_builder:extend (builder)
end

--- The build function, for building anything!
function build (builder)
	int.assert_quit (type (tostring (builder.input)) == 'string',
			"Can't build something without a valid input field.\
Needed a string, got a " .. type (builder.input) .. '.', 2)
	-- if called build function explicitly, search for the builder
	-- defaults to it's extension, or fallback to copy
	if getmetatable (builder) ~= 'hellbuilder' then
		builder = getDefaultBuilder (builder)
	end
	-- the new build
	local new_cmd = util.substField (builder, 'cmd')
	local new = {
		__metatable = 'build',
		echo = builder.echo,
		input = builder.input,
		output = builder.output or builder.input,
		cmd = new_cmd:gsub (builder.input, util.curryPrefixEach (table.concat (int.path, '/', 2)))
	}
	if hell.outdir then
		new.cmd = new.cmd:gsub (builder.output, hell.outdir .. '/' .. builder.output)
	end
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
