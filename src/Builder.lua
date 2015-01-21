--[[		Builder: the heart of the Hell build scripts		]]--

require 'build_install'

--- Auxiliary function for merging two fields
--
-- Fields are concatenated if the second starts with a '&',
-- and substituted otherwise (the '&' may be escaped with a '!').
-- If the second doesn't exist, use the original one.
--
-- @param[in] target The first field, to merged with
-- @param[in] src The second field, for merging with the first
-- @param[in] sep The separator to be used when concatting. Default = ' '
--
-- @return The merged fields
local function mergeFields (target, src, sep)
	sep = sep or ' '
	-- if src is a string, we may want to concatenate
	if type (src) == 'string' then
		local prefix, sufix = src:sub (1, 1), src:sub (2)
		if prefix == '&' then
			return target .. sep .. sufix
		elseif prefix == '!' then
			return sufix
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
	appendix = appendix or {}
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

-- Builder's access to the methods
builder.__index = builder

--- If you want to put a builder inside a builder, extend it
function builder.__newindex (t, k, v)
	if getmetatable (v) == builder then
		rawset (t, k, t:extend (v))
	else
		rawset (t, k, v)
	end
end

function builder.__call (self, t)
	return build (self:extend (t))
end

-- If we help hellbuilds, we are a...
builder.__metatable = 'hellbuilder'

--- Builder constructor
--
-- @param[in] initializer A table with the fields for initializing the builder.
--	Fields will be extended.
--
-- @return A new Builder
function Builder (initializer)
	initializer = initializer or {}
	local new = {}
	setmetatable (new, builder)
	-- copy the cmd field from builder, for the extend to work right
	new.cmd = builder.cmd
	-- get the fields from initializer
	new = new:extend (initializer)

	return new
end

--[[		Load all builders from the builders directory		]]--
local function buildersIter ()
	local dir = io.popen ('ls builders/*.lua')
	return function ()
		return dir:read ()
	end
end

for f in buildersIter () do
	dofile (f)
end
