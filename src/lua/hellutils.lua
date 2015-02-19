--- @file hellutils.lua
-- Auxiliary functions for hell users. This will be `require`d in the
-- modules where needed, so it will reside somewhere in the 'package.path'.
-- Enjoy the various facilities we're providing xD

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
-- @note Currently it uses only `ls`, so don't really use it in windows
-- (I'll use `dir` someday, and make it more crossplatform)
--
-- @param pattern The search pattern
--
-- @return Table with all filename matches
function t.glob (pattern)
	return int.hs.glob (pattern, int.getPath ())
end


--- Prefix each word in str with prefix
--
-- @param str The string
-- @param prefix The prefix to be used
--
-- @return The string str with prefix before every word
function t.prefixEach (str, prefix)
	return str:gsub ('%S+', prefix .. '%1')
end


--- Prefix `str' with `prefix', if it ain't already prefixed by it.
--
-- @param str The string
-- @param prefix The prefix to be used if needed
--
-- @return The final string
function t.lazyPrefix (str, prefix)
	return int.hs.lazyPrefix (str, prefix)
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
-- @return table with the results
function t.fmap (field, f)
	int.assert_quit (type (f) == 'function', "[fmap] Can't map a function if it ain't a function!", 2)

	if type (field) ~= 'table' then
		return { f (field) }
	end

	-- apply function over every value in field, keeping changes
	local results = {}
	for i, v in ipairs (field) do
		results[i] = f (v)
	end

	return results
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
	return table.concat (t.fmap (field, t.id), ' ')
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
			-- call the field's 'prepare_' function, if it exists, or return the
			-- field, or an empty string
			local prepare = builder['prepare_' .. capture]
			if prepare then
				builder[capture] = prepare (builder[capture], builder)
				builder['prepare_' .. capture] = nil
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
function t.getNestedField (t, field)
	if field == '' then
		return t
	else
		return getNestedField (t, field)
	end
end


--- Get the build path, it's important for the commands to be executed 
function t.getBuildPath (builder)
	local str = ''
	if hell.outdir then
		str = hell.outdir .. hell.os.dir_sep
	end

	if hell.keepDirStructure or builder.keepDirStructure then
		str = str .. int.getPath (2)
	end

	return str
end


return t
