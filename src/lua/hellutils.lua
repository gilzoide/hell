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

-- `hs' is the forward declaration of the haskell library that `hell.lua'
-- will give us
local t = {}


function t.cloneTable (src)
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
function t.glob (pattern)
	local dir = int.getPath ()
	if dir == '' then
		dir = './'
	end
	return int.cpp.glob (dir .. pattern)
end


--- Maps a function over a table
--
-- @param field The target
-- @param f The function
-- @param force_table Bool: do you want the result to be a table, even if
--  `target' ain't a table?
--
-- @return table with the results
function t.fmap (field, f, force_table)
	int.assert_quit (type (f) == 'function', "[fmap] Can't map a function if it ain't a function!", 2)

	if type (field) ~= 'table' then
		return force_table and { f (field) } or f (field)
	end

	-- apply function over every value in field, keeping changes
	local results = {}
	for i, v in ipairs (field) do
		results[i] = f (v)
	end

	return results
end


--- Prefix a string `str' with `prefix'
function t.prefix (str, prefix)
	return prefix .. str
end


--- Curried version of `prefix' function
function t.curryPrefix (prefix)
	return function (str) return t.prefix (str, prefix) end
end


--- Prefix `str' with `prefix', if it ain't already prefixed by it.
--
-- @param str The string
-- @param prefix The prefix to be used if needed
--
-- @return The final string
function t.lazyPrefix (str, prefix)
	return int.cpp.lazyPrefix (str, prefix)
end

--- Curry the `lazyPrefix' function, just as @ref curryPrefix does
function t.curryLazyPrefix (prefix)
	return function (str) return t.lazyPrefix (str, prefix) end
end


--- Function for doing nothing
--
-- It's useful for passing it into fmap, when you don't want to mess with things
function t.id (a)
	return a
end


--- If field is a table, table.concat it; don't do a thing otherwise
--
-- @param field The field
--
-- @return The field unpacked and concatenated, or unaltered
function t.concat (field)
	return table.concat (t.fmap (field, t.id, true), ' ')
end


--- Changes a file name's extension to the `new' one
function t.changeExtension (file_name, new)
	-- drop the dot from filename, if there should be no extension
	if new ~= '' then
		new = '.' .. new
	end
	return file_name:gsub ('(%.?.-)%..*', '%1' .. new)
end


--- Substitute the fields in a command
--
-- @note When a field from t is nil, it's entry is substituted with ''
-- (just like shell would do).
-- @note This function updates the fields in the builder, aswell as consumes
-- the `prepare_*' functions, so be careful!
--
-- @param builder The table with the fields
-- @param str The string to be substituted
--
-- @return A string with the substituted stuff
function t.subst (builder, str)
	int.assert_quit (type (str) == 'string', "[subst] Can't substitute parameter: it isn't a string", 3)

	-- build the command substituting anything that starts with a '$'
	-- (unless it's escaped with another '$')
	local function sub (capture)
		if capture:sub (1, 1) == '$' then
			return capture
		else
			return t.concat (builder[capture]) or ''
		end
	end

	return str:gsub ('$([$%w_]+)', sub)
end


--- Wrapper for the subst function, which uses a builder's field as string
--
-- @param builder The table with the fields
-- @param field Builder's field to be substituted
--
-- @return A string with the substituted stuff
function t.substField (builder, field)
	return t.subst (builder, builder[field])
end


--- Auxiliary recursion function for t.getNestedField
local function getNestedField (t, field)
	if not field or not t then
		return t
	else
		local current, rest = field:match ('(.-)%.(.+)')
		current = current or field
		return getNestedField (t[current], rest)
	end
end
--- Gets the nested field inside table t.
--
-- It works by recursing over tables until there's no more '.' in field name.
--
-- @param t The table which will be searched
-- @param field 
--
-- @return Field asked for, whole table if empty field name
function t.getNestedField (t, field)
	if field == '' then
		return t
	else
		return getNestedField (t, field)
	end
end


--- Take filename from filepath (until the last os.separator)
function t.takeFileName (filename)
    return filename:match ('.*' .. hell.os.dir_sep .. '(.+)') or filename
end


function t.makeRelative (path, base)
	-- first of all, lazyPrefix base path
	local prefixed = t.lazyPrefix (path, base)
	local ret = {}
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


return t
