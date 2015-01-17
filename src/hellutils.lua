--- @file hellutils.lua
-- Auxiliary functions for hell users. This will be `require`d in the
-- modules where needed, so it will reside somewhere in the 'package.path'.
-- Enjoy the various facilities we're providing xD

local t = {}

--- Prefix each word in str with prefix
--
-- @param[in] str The string
-- @param[in] prefix The prefix to be used
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
-- @param[in] prefix The prefix to be curried in prefixEach
--
-- @return prefixEach's result
function t.curryPrefixEach (prefix)
	return function (str) return t.prefixEach (str, prefix) end
end

--- If field is a table, table.concat it; don't do a thing otherwise
--
-- @param[in] field The field
--
-- @return The field unpacked and concatenated, or unaltered
function t.concat (field)
	return type (field) == 'table' and table.concat (field, ' ') or field
end

return t
