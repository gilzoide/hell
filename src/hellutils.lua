--- @file hellutils.lua
-- Auxiliary functions for hell users. This will be `require`d in the
-- modules where needed, so it will reside somewhere in the 'package.path'.
-- Enjoy the various facilities we're providing xD

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

--- If field is a table, table.concat it; don't do a thing otherwise
--
-- @param field The field
--
-- @return The field unpacked and concatenated, or unaltered
function t.concat (field)
	return type (field) == 'table' and table.concat (field, ' ') or field
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
	assert_quit (type (str) == 'string', "[subst] Can't substitute parameter: it isn't a string", 3)

	-- build the command substituting anything that starts with a '$'
	-- (unless it's escaped with another '$')
	local function sub (capture)
		if capture:sub (1, 1) == '$' then
			return capture
		else
			-- call the field's 'prepare_' function, if it exists, or return the
			-- field, or an empty string
			local field, prepare = builder[capture], builder['prepare_' .. capture]
			return prepare and prepare (field, builder) or field or ''
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

return t
