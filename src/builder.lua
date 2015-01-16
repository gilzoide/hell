--[[	Builder: the heart of the Hell build scripts	]]--

--- Auxiliary function for merging two fields
--
-- Fields are concatenated if the second starts with a '&',
-- and substituted otherwise. If the second doesn't exists,
-- use the original one.
--
-- @param[in] target The first field, to merged with
-- @param[in] src The second field, for merging with the first
--
-- @return The merged fields
local function mergeFields (target, src)
	-- if src is a string, we may want to concatenate
	if type (src) == 'string' then 
		if src:sub (1, 1) == '&' then
			return target .. src:gsub ('&', ' ', 1)
		elseif src:sub (1, 1) == '!' then
			return src:sub (2)
		else
			return src
		end
	-- well, return anyone who ain't nil
	elseif src ~= nil then
		return src
	else
		return target
	end
end

-- the builder metatable
local builder = {}

--- The only mandatory field in a builder: the command to be run
builder.cmd = ''

--- Extending a builder is easy, just call this method!
--
-- @note This method doesn't change the original builder,
-- but rather returns a new one with the extended fields
function builder:extend (appendix)
	local new = {}
	setmetatable (new, builder)
	-- merge fields from original builder
	for k, v in pairs (self) do
		new[k] = mergeFields (v, appendix[k])
	end
	-- and get the new fields from the appendix
	for k, v in pairs (appendix) do
		new[k] = new[k] or v
	end

	return new
end

--- default fields for the new builders
builder.__index = builder

--- If you want to put a builder inside a builder, extend it
builder.__newindex = function (t, k, v)
	if getmetatable (v) == builder then
		rawset (t, k, t:extend (v))
	else
		rawset (t, k, v)
	end
end

--- Builder constructor
--
-- @param[in] initializer A table with the fields for initializing the builder.
--	Fields can be extended.
--
-- @return A new Builder
function Builder (initializer)
	initializer = initializer or {}
	local new = {}
	setmetatable (new, builder)
	-- get the fields from parent
	new = new:extend (initializer)

	return new
end
