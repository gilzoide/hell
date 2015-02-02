--- @file hellutils.lua
-- Auxiliary functions for hell users. This will be `require`d in the
-- modules where needed, so it will reside somewhere in the 'package.path'.
-- Enjoy the various facilities we're providing xD

local int = require 'internals'

local t = {}

--- Prefix each word in str with prefix
--
-- @param str The string
-- @param prefix The prefix to be used
--
-- @return The string str with prefix before every word
function t.prefixEach (str, prefix)
	return str:gsub ('%S+', prefix .. '%1')
end


--- Curry the prefixEach function with the prefix
--
-- For preparing a field, it's often useful to just prefix it. Rewriting the 
-- function for calling it everytime is boring, so here it is!
--
-- __Example__:
-- 		prepare_flags = curryPrefixEach ('-')
-- 		prepare_includes = curryPrefixEach ('-I')
-- 		prepare_defines = curryPrefixEach ('-D')
--
-- @param prefix The prefix to be curried in prefixEach
--
-- @return prefixEach's result
function t.curryPrefixEach (prefix)
	return function (str) return t.prefixEach (str, prefix) end
end

--- Maps a function over a table
--
-- @return Strings with the results concatenated
function t.fmap (field, f)
	int.assert_quit (type (f) == 'function', "[fmap] Can't map a function if it ain't a function!", 2)

	if type (field) ~= 'table' then
		return f (field)
	end

	-- apply function over every value in field, keeping changes
	local results = {}
	for i, v in ipairs (field) do
		results[i] = f (v)
	end

	return table.concat (results, ' ')
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
	return t.fmap (field, t.id)
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
			-- call the field's 'prepare_' function, if it exists, or return the
			-- field, or an empty string
			local prepare = builder['prepare_' .. capture]
			if prepare then
				builder[capture] = prepare (builder[capture], builder)
			end
			return builder[capture] or ''
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

--- Auxiliary function: gets the nested field inside table t.
--
-- It works by recursing over tables until there's no more '.' in field name.
--
-- @param t The table which will be searched
-- @param field 
local function getNestedField (t, field)
	if not field or not t then
		return t
	else
		local current, rest = field:match ('(.-)%.(.+)')
		current = current or field
		return getNestedField (t[current], rest)
	end
end
t.getNestedField = getNestedField

return t
