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

local int = require 'hell.internals'

local BI = {}

BI.builds = {}
BI.installs = {}
BI.targets = {}

--- Auxiliary function for getting the builds/installs from a table
function BI.getBI (t, meta)
	if type (t) ~= 'table' then
		return nil
	elseif getmetatable (t) == meta then
		return {t}
	end

	local ret = {}
	for i, v in ipairs (t) do
		if getmetatable (v) == meta then
			table.insert (ret, v)
		end
	end

	return ret
end


--- Gets the default builder, based on input's extension
local function getDefaultBuilder (builder)
	local auto_builder
	if not builder.builder then
        local input_example
		if type (builder.input) == 'table' then
			input_example = builder.input[1]
		else
			input_example = builder.input or ''
		end

		local ext = input_example:match ('.-%.(%S+)')
		auto_builder = _ENV[ext] or copy
	else
		int.assert_quit (getmetatable (builder.builder) == 'hellbuilder',
				"Trying to use an invalid Builder")
		-- calling build with explicit builder field
		auto_builder = builder.builder
	end
	return auto_builder:extend (builder)
end


--- The build function, for building anything!
local function _build (builder)
	-- both input and outputs will have, apart from the original "prepare_"
	-- function, another one. Thus we need to apply the original within the new
	-- one
	local original_prepare_input = builder.prepare_input or utils.id
	local original_prepare_output = builder.prepare_output or utils.id

	local function new_prepare_input (i, b)
		local parcial_result = original_prepare_input (i, b)
		int.assert_quit (parcial_result, "No input found in builder, even after preparing")
		-- only prefix input path if it's not a pipeBuild
		if not builder.pipe then
			parcial_result = utils.fmap (utils.curry (utils.makeRelative, int.getPath ()), parcial_result)
		end
		return utils.fmap (utils.id, parcial_result, true)
	end
	local function new_prepare_output (o, input)
		local parcial_result = original_prepare_output (o, input)
		return utils.makeRelative (int.getBuildPath (builder), parcial_result or input)
	end
	
	-- call all the "prepare_" functions, starting with "output"
	-- (output is special, cuz we need to prepare the output first,
	-- as it often is based on the input field, that's why it's called first)
	--
	-- @note prepare_input is the only "prepare_" function called with the 
	--  builder as parameter, as it might do some pipeBuilds
	builder.input = new_prepare_input (builder.input, builder)
	builder.prepare_input = nil
	-- @note that input_filename == fileName only if not keepDirStructure
	local input_filename = utils.takeFilename (builder.input[1])
	builder.output = new_prepare_output (builder.output, input_filename)
	builder.prepare_output = nil
	-- find "prepare_" functions...
	local prepare_funcs = {}
	for k, v in pairs (builder) do
		local field_name = k:match ('prepare_(.+)')
		-- there's a match, and `v' is not false (see the second
		-- @note in `builder:extend` function)
		if field_name and v then
			prepare_funcs[field_name] = v
		end
	end
	-- ...and run them
	--
	-- @note Was running "prepare_" functions and updating builder while
	--  iterating the table, which was causing random bugs, like some
	--  functions being called twice, or even never =S
	for field, func in pairs (prepare_funcs) do
		-- try to run the function
		local status, ret = pcall (func, builder[field], input_filename)
		-- in case anything went wrong, tell us right away!
		if not status then
			int.assert_quit (false, table.concat {
				"Error on builder \'prepare_", field, "\' function: \"", ret, '"'
			}, true)
		end
		builder[field] = ret
	end
	-- the new build
	local new_cmd = utils.subst (builder.cmd, builder)

	local new = {
		__metatable = 'build',
		echo = builder.echo,
		-- clone the table, so that any dependency addition isn't reflected in
		-- the original one
		deps = utils.cloneTable (builder.deps),
		input = builder.input,
		output = builder.output,
		cmd = new_cmd,
		--- Adds a dependency into build
		--
		-- @param dep Dependency to add, either a string or another build
		addDep = function (self, dep) table.insert (self.deps, dep) end
	}
	setmetatable (new, new)

	-- only push build if there's a non blank command
	if new.cmd:match ('%S') then
		table.insert (BI.builds, new)
	end

	return new
end


--- Pipe a `source' build into `target' build
--
-- A "pipe build" is a build that will be piped into the `target' build.
-- That means `source' will be built as a dependency for `target', and will
-- not be returned, so that only `target' has its reference.
--
-- @warning Pipebuild doesn't append target's dependencies
--
-- @return Source's output file name, as it may be needed for setting `target's
--  input
function pipeBuild (target, source)
	int.assert_quit (getmetatable (target) == 'hellbuilder',
			"Can't pipe a build into something that ain't a hellbuilder.")


	source.pipe = false
	local new = _build (target:extend (source))

	-- flag that shows `target' is formed by pipe builds
	target.pipe = true
	table.insert (target.deps, new)

	return new.output
end


--- @ref _build wrapper, for multibuilds
--
-- @return All processed builds, more than one if multinput
function build (builder)
	-- if called build function explicitly, search for the builder
	-- defaults to it's extension, or fallback to copy
	if getmetatable (builder) ~= 'hellbuilder' then
		builder = getDefaultBuilder (builder)
	end
	-- we need a command, man
	int.assert_quit (type (builder.cmd) == 'string',
			"Can't build something without a command.\
Needed a string, got a " .. type (builder.cmd) .. '.')

	local all_builds
	-- support for multinput: 
	if builder.multinput then
		all_builds = utils.fmap (function (i) 
			return builder:extend { input = i }
		end, builder.input, true)
	else
		all_builds = { builder }
	end

	-- return all builds unpacked (if not multinput, return the only
	-- build as it would normally do
	return table.unpack (utils.fmap (_build, all_builds))
end


--- The install function
--
-- @note Build will be installed in "$(prefix)/$(dir)" directory;
--  prefix can come directly from command line options, or use the OS default.
--
-- @param in_build The input: a build
-- @param dir Install in_build into which directory (relative to prefix)?
-- @param permission Unix `install` permission. Windows ignores it. Default: 755
local function _install (in_build, dir, permission)
	int.assert_quit (getmetatable (in_build) == 'build',
			"Can't install something that's not a build")
	int.assert_quit (type (dir) == 'string',
			"Can't install without knowing where to (second parameter should be a string).")
	dir = utils.makeRelative ((prefix or hell.os.prefix) .. (dir ~= '' and hell.os.dir_sep or ''), dir)

	local filename = utils.takeFilename (in_build.output)

	--- Install Builder: the one that installs stuff (in Windows, it's just copy)
	local installBuilder = hell.os.name == 'windows' and copy or Builder {
		bin = 'install',
		permission = permission or '755',
		cmd = '$bin -m $permission $input $output'
	}

	-- create install dir, if needed
	int.cpp.createDirIfNeeded (dir)

	-- install's input and output
	builder = installBuilder:extend {
		input = { in_build.output },
		output = utils.makePath (dir, filename)
	}

	local new = {
		__metatable = 'install',
		input = builder.input,
		output = builder.output,
		deps = {},
		cmd = utils.subst (builder.cmd, builder)
	}
	setmetatable (new, new)

	table.insert (BI.installs, new)
	return new
end


--- @ref _install wrapper, for multiinstalls
--
-- @return All processed installs, more than one if in_builds is a table
function install (in_builds, dir, permission)
	if int.opts.command == 'install' or int.opts.command == 'uninstall' then
		local all_installs
		-- if `in_builds' isn't a build, we suppose it's a table containing builds
		if getmetatable (in_builds) ~= 'build' then
			all_installs = in_builds
		else
			all_installs = { in_builds }
		end

		local function curryInstall (in_build)
			return _install (in_build, dir, permission)
		end

		return table.unpack (utils.fmap (curryInstall, all_installs))
	end
end


--- Makes `tbl' a target, adding it to the `BI.targets' table
function target (name, tbl)
	int.assert_quit (type (name) == 'string', 'Target name should be a string')
	int.assert_quit (type (tbl) == 'table', 'Target should be a build, or a table with builds')
	BI.targets[name] = tbl
end


--- Makes `tbl' a target, cleaning it's command if it ain't the chosen target
function exclusiveTarget (name, tbl)
	if utils.getOption 'target' ~= name then
		-- ensure tbl is a table
		if getmetatable (tbl) == 'build' then
			tbl = { tbl }
		end
		--- Function that clears the cmd and dependencies
		local function clearCmd (t)
			t.cmd = ''
			t.deps = {}
		end
		utils.fmap (clearCmd, tbl)
	end
	target (name, tbl)
end


--- List all targets, which are saved in `BI.targets'
function BI.listTargets ()
	for name, _ in pairs (BI.targets) do
		print (name)
	end
end


--- Function for transforming builds in cleans
function BI.makeClean (builds)
	-- table with the processed builds, so that we don't process any build 
	-- twice; saves us from falling into endless dependency cycles
	local processed = {}
	-- table with the cleaning builds, which will be returned
	local cleans = {}

	--- Remove Builder: the one used for cleaning
	local remove = Builder {
		bin = hell.os.name == 'windows' and 'del' or 'rm -rf',
		prepare_output = function (_, input) return input end,
		-- force pipe build, so that it doesn't mess with our input (which
		-- is already the full name for the output we want to clean)
		pipe = true,
		cmd = '$bin $input'
	}

	-- if called hell with the -c option, just clean the outdir
	if int.opts.c and hell.outdir then
		table.insert (cleans, remove { input = hell.outdir })
	else
		--- Function for removing a build, and it's dependencies recursively
		--
		-- @note By removing, we mean creating a "build" that will remove the 
		-- build given by argument
		local function removeBuild (b)
			if type (b) ~= 'table' then return nil end

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
	end

	return cleans
end


return BI
