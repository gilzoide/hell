--- @file build_install.lua
-- build and install functions, vital pieces of the hell build system

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
        local input_example
		if type (builder.input) == 'table' then
			input_example = builder.input[1]
		else
			input_example = builder.input
		end

		local ext = input_example:match ('.-%.(%S+)')
		auto_builder = _ENV[ext] or copy
	else
		int.assert_quit (getmetatable (builder.builder) == 'hellbuilder',
				"Trying to use an invalid Builder", 2)
		-- calling build with explicit builder field
		auto_builder = builder.builder
	end
	return auto_builder:extend (builder)
end


local function _build (builder)
	-- both input and outputs will have, apart from the original "prepare_"
	-- function, another one. Thus we need to apply the original within the new
	-- one
	local original_prepare_input = builder.prepare_input or util.id
	local original_prepare_output = builder.prepare_output or util.id

	local new_prepare_input = function (i, b)
		local parcial_result = original_prepare_input (i, b)
		-- only prefix input path if it's not a pipeBuild
		if not builder.pipe then
			parcial_result = util.fmap (parcial_result, function (i) 
						return util.makeRelative (i, int.getPath ())
					end)
		end
		return util.fmap (parcial_result, util.id, true)
	end
	local new_prepare_output = function (o, input)
		local parcial_result = original_prepare_output (o, input)
		return util.makeRelative (parcial_result or input, int.getBuildPath (builder))
	end
	
	-- call all the "prepare_" functions, starting with "output"
	-- (output is special, cuz we need to prepare the output first,
	-- as it often is based on the input field, that's why it's called first)
	--
	-- @note prepare_input is the only "prepare_" function called with the 
	-- builder as parameter, as it might do some pipeBuilds
	builder.input = new_prepare_input (builder.input, builder)
	builder.prepare_input = nil
	local input_filename = util.takeFileName (builder.input[1])
	builder.output = new_prepare_output (builder.output, input_filename)
	builder.prepare_output = nil
	-- and the other ones
	for k, v in pairs (builder) do
		local capture = k:match ('prepare_(.+)')
		if capture and v then
			builder[capture] = v (builder[capture], input_filename)
			builder[k] = nil
		end
	end
	-- the new build
	local new_cmd = util.substField (builder, 'cmd')

	local new = {
		__metatable = 'build',
		echo = builder.echo,
		deps = builder.deps,
		input = builder.input,
		output = builder.output,
		cmd = new_cmd,
		addDep = function (self, dep) table.insert (self.deps, dep) end
	}
	setmetatable (new, new)

	table.insert (BI.builds, new)
	return new
end


--- Pipe a `source' build into `target' build
--
-- A "pipe build" is a build that will be piped into the `target' build.
-- That means `source' will be built as a dependency for `target', and will
-- not be returned, so that only `target' has its reference.
--
-- @return Source's output file name, as it may be needed for setting `target's
--  input
function pipeBuild (target, source)
	int.assert_quit (getmetatable (target) == 'hellbuilder',
			"Can't pipe a build into something that ain't a hellbuilder.", 2)

	source.pipe = false
	local new = _build (target:extend (source))

	target.pipe = true
	table.insert (target.deps, new)
	-- flag that shows `target' is formed by pipe builds
	return new.output
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
Needed a string, got a " .. type (builder.cmd) .. '.', 2)

	local all_builds
	-- support for multinput: 
	if builder.multinput then
		all_builds = util.fmap (builder.input, function (i) 
			return builder:extend { input = i }
		end, true)
	else
		all_builds = { builder }
	end

	-- return all builds unpacked (if not multinput, return the only
	-- build as it would normally do
	return table.unpack (util.fmap (all_builds, _build))
end

--- The install function
--
-- @note Build will be installed in "$(prefix)/$(dir)" directory.
-- prefix can come directly from command line options, or use the OS default.
function install (builder, dir)
	int.assert_quit (type (dir) == 'string',
		"Can't install without knowing where to (second parameter should be a string).", 2)
	dir = (prefix or hell.os.prefix) .. hell.os.dir_sep .. dir
	int.assert_quit (type (builder.output or builder.input) == 'string',
			"Can't install without output or input fields.", 2)

	if builder.cmd then
		util.substField (builder, 'cmd')
	end

	builder = copy:extend {
		input = builder.output or builder.input,
	}
	builder.output = dir .. hell.os.dir_sep .. builder.input

	local new = {
		__metatable = 'install',
		input = builder.input,
		output = builder.output,
		deps = builder.deps,
		cmd = util.substField (builder, 'cmd')
	}
	setmetatable (new, new)

	table.insert (BI.installs, new)
	return new
end


--- Function for transforming builds in cleans
function BI.makeClean (builds)
	-- table with the processed builds, so that we don't process any build 
	-- twice; saves us from falling into endless dependency cycles
	local processed = {}
	-- table with the cleaning builds, which will be returned
	local cleans = {}

	local remove = Builder {
		bin = hell.os.name == 'windows' and 'del' or 'rm -f',
		prepare_output = function (_, input) return input end,
		-- force pipe build, so that it doesn't mess with our input (which
		-- is already the full name for the output we want to clean)
		pipe = true,
		cmd = '$bin $input'
	}
	--- Function for removing a build, and it's dependencies recursively
	--
	-- @note By removing, we mean creating a "build" that will remove the 
	-- build given by argument
	local function removeBuild (b)
		if type (b) ~= 'table' then return nil end

		--print (b.input)

		local target = b.output
		-- not processed yet, let's go!
		if not processed[target] then
			processed[target] = true

			-- dependencies first
			for _, bb in ipairs (b.deps) do
				table.insert (cleans, removeBuild (bb))
			end

			return remove { input = target }
		else
			return nil
		end
	end

	for _, b in ipairs (builds) do
		table.insert (cleans, removeBuild (b))
	end

	return cleans
end


return BI
