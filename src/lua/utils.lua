--- @file hellutils.lua
-- Auxiliary functions for hell users. This will be `require`d in the
-- modules where needed, so it will reside somewhere in the 'package.path'.
-- Enjoy the various facilities we're providing xD

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

utils = {}

--- Gets hell's version
function utils.getVersion ()
	return int.version
end


--- Gets current working directory's absolute path
function utils.getcwd ()
	return int.cpp.getcwd ()
end


--- Gets root hellbuild path
function utils.getPath ()
	return int.getPath ()
end


--- Get option value from `int.opts` table
--
-- @note Valid `opt' values (which don't always return nil):
--   - any short options described in "hellp.lua"
--   - command ('build', 'install', 'clean', 'uninstall')
--   - target (the most useful one, IMHO)
function utils.getOption (opt)
	return int.opts[opt]
end


--- Clones a table (copying each field)
--
-- @note This is a shallow clone, so any tables inside 
--  will be copied by reference
--
-- @return The clone table
function utils.cloneTable (src)
	local new = {}
	for k, v in pairs (src) do
		new[k] = v
	end
	return new
end


--- Glob function, returns all matches in a table
--
-- @param pattern The search pattern
--
-- @return Table with all filename matches
function utils.glob (pattern)
	return int.cpp.glob (pattern)
end


--- Recursive glob function, ran over every directory inside first
--
-- @warning This may be dangerous if there's any circular link!
--
-- @param pattern The pattern to be matched
-- @param base Base path, where it all starts. Default = '.'
--
-- @return All matches, from within all directories
function utils.recursiveGlob (pattern, base)
	local function recGlob (pattern, base)
		-- first, get every dir there is
		local allDirs = utils.glob (base .. '*/')

		local allMatches = {}
		for _, dir in ipairs (allDirs) do
			-- keep matches until now, adding the ones from the next directory
			for _, file in ipairs (recGlob (pattern, dir)) do
				table.insert (allMatches, file)
			end
		end

		-- add now matches from current directory
		for _, file in ipairs (utils.glob (base .. pattern)) do
			table.insert (allMatches, file)
		end

		return allMatches
	end

	base = base or '.'
	return recGlob (pattern, utils.lazyPrefix (base, hell.os.dir_sep))
end


--- Maps a function over a table
--
-- @param f The function
-- @param t The target table
-- @param force_table Bool: do you want the result to be a table, even if
--  `t' ain't a table?
--
-- @return table with the results
function utils.fmap (f, t, force_table)
	int.assert_quit (type (f) == 'function', "[fmap] Can't map a function if it ain't a function!")

	if type (t) ~= 'table' then
		return force_table and { f (t) } or f (t)
	end

	-- apply function over every value in `t', keeping changes
	local results = {}
	for i, v in ipairs (t) do
		results[i] = f (v)
	end

	return results
end


--- Curries a function `func' with the arguments, applying it partially
-- 
-- Functional programming stuff =]
--
-- @param func The function that'll be partially applied
-- @param ... The arguments to be applied
--
-- @return The new partially applied function
function utils.curry (func, ...)
	local applied_args = {...}
	return function (...) return func (table.unpack (applied_args), ...) end
end


--- Prefix a string `str' with `prefix'
function utils.prefix (str, prefix)
	return prefix .. str
end


--- Prefix `str' with `prefix', if it ain't already prefixed by it.
--
-- @param prefix The prefix to be used if needed
-- @param str The string
--
-- @return The final string
function utils.lazyPrefix (prefix, str)
	return int.cpp.lazyPrefix (prefix, str)
end


--- Function for doing nothing
--
-- It's useful for passing it into fmap, when you don't want to mess with things
function utils.id (a)
	return a
end


--- If field is a table, table.concat it; don't do a thing otherwise
--
-- @param field The field
--
-- @return The field unpacked and concatenated, or unaltered
function utils.concat (field)
	return table.concat (utils.fmap (utils.id, field, true), ' ')
end


--- Extends table `dst' with all values in `src'
-- Lua 5.3 provides us with the `table.move` function, which serves us well,
-- so use it
--
-- @param src Source table, from which the items will be read
-- @param dst Destiny table, to whom the items will be appended
utils.extendTable = _VERSION == 'Lua 5.3'
-- Lua 5.3 version
and function (src, dst)
	return table.move (src, 1, #src, #dst + 1, dst)
end
-- Lua 5.2 version
or function (src, dst)
	for _, v in ipairs (src) do
		table.insert (dst, v)
	end

	return dst
end


--- Changes a `file_name's extension to the `new_ext' one
function utils.changeExtension (new_ext, file_name)
	-- drop the dot from filename, if there should be no extension
	if new_ext ~= '' then
		new_ext = '.' .. new_ext
	end
	return file_name:gsub ('(%.?.-)%..*', '%1' .. new_ext)
end


--- Substitute the fields in a command
--
-- @note When a field from t is nil, it's entry is substituted with ''
--  (just like shell would do).
-- @note This function updates the fields in the builder, aswell as consumes
--  the `prepare_*' functions, so be careful!
--
-- @param builder The table with the fields
-- @param str The string to be substituted
--
-- @return A string with the substituted stuff
function utils.subst (str, builder)
	int.assert_quit (type (str) == 'string', "[subst] Can't substitute parameter: it isn't a string")

	-- build the command substituting anything that starts with a '$'
	-- (unless it's escaped with another '$')
	local function sub (capture)
		if capture:sub (1, 1) == '$' then
			return capture
		else
			return utils.concat (builder[capture]) or ''
		end
	end

	return str:gsub ('$([$%w_]+)', sub)
end


--- Wrapper for the subst function, which uses a builder's field as string
--
-- @param field Builder's field to be substituted
-- @param builder The table with the fields
--
-- @return A string with the substituted stuff
function utils.substField (field, builder)
	return utils.subst (builder[field], builder)
end


--- Gets the nested field inside table t.
--
-- It works by recursing over tables until there's no more '.' in field name.
--
-- @param field Field name (string)
-- @param t The table which will be searched
--
-- @return Field asked for, whole table if empty field name
function utils.getNestedField (field, t)
	--- Auxiliary recursion function for getNestedField
	local function recGetNestedField (field, t)
		if not field or not t then
			return t
		else
			local current, rest = field:match ('(.-)%.(.+)')
			current = current or field
			return recGetNestedField (rest, t[current])
		end
	end


	if field == '' then
		return t
	else
		return recGetNestedField (field, t)
	end
end


--- Take filename from filepath (until the last os.dir_sep)
function utils.takeFilename (filename)
    return filename:match ('.*' .. hell.os.dir_sep .. '(.+)') or filename
end


--- Take directory name (until last os.dir_sep)
function utils.takeDirectory (path)
	return path:match ("(.+)" .. hell.os.dir_sep .. ".+")
end


--- Makes a path by concatting the various paths with dir_sep
function utils.makePath (...)
	return table.concat ({...}, hell.os.dir_sep)
end


--- Finds a relative path between `path' and `base' paths
--
-- @param base Base file path, used as reference
-- @param path The file path to be processed
--
-- @returns Relative path
function utils.makeRelative (base, path)
	-- first of all, lazyPrefix base path
	local prefixed = utils.lazyPrefix (base, path)
	-- return value. If prefixed starts with a '/', don't discard it
	local ret = { prefixed:sub (1, 1) == '/' and '' or nil }
	-- now, remove any combination "dir/../", in any level
	for dir in prefixed:gmatch '([^/]+)/?' do
		if dir ~= '.' then
			if dir == '..' and ret[#ret] and ret[#ret] ~= '..' then
				table.remove (ret)
			else
				table.insert (ret, dir)
			end
		end
	end

	return table.concat (ret, hell.os.dir_sep)
end


--- Runs a command on the shell, and returns its output
--
-- @param command Command to be run
--
-- @return Command's output, or nil if no output is read
function utils.shell (command, showErr)
	-- open the command file handler, suppressing its stderr if needed
	local handler = io.popen (command .. (showErr and '' or ' 2> /dev/null'))
	-- read everything from command run
	local ret = handler:read ('*a')
	-- and close the command file handler
	handler:close ()

	-- if command failed, reads nothing, so return nil
	if ret ~= '' then
		return ret:sub (1, -2)
	else
		return nil
	end
end


return utils
