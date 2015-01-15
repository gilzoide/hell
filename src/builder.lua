--[[	Builder: the heart of the Hell build scripts	]]--

-- the builder metatable
local builder = {
	cmd = ''	--- The only mandatory field in a builder: the command to be run
}

--- Builder constructor
--
-- @param[in] initializer A table with the fields for initializing the builder.
--	Fields can be extended.
--
-- @return A new Builder
function newBuilder (initializer)
	parent = parent or {}
	local new = {}
	setmetatable (new, builder)
	-- get the fields from parent
	copyExtendibleFields (new, initializer)

	return new
end
