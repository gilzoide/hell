--- Substitute the fields in a command
--
-- @note When a field from t is nil, it's entry is substituted with ''
-- (just like shell would do).
--
-- @param[in] t The table with the fields
-- @param[in] cmd The command to be formed
--
-- @return A string with the right command
local function subst (builder, field)
	assert_quit (type (builder[field]) == 'string', "[subst] Can't substitute builder's " .. field .. ": it isn't a string", 3)

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

	return builder[field]:gsub ('$([$%w_]+)', sub)
end

--- The build function, for building anything!
function build (builder)
	assert_quit (type (builder.input) == 'string', "Can't build something without an input field", 2)
	-- if called build function explicitly, search for the builder
	-- defaults to it's extension, or fallback to copy
	if getmetatable (builder) ~= 'hellbuilder' then
		local auto_builder
		if not builder.builder then
			local ext = builder.input:match ('.-%.(%S+)') 
			auto_builder = _ENV[ext] or copy
		else
			assert_quit (getmetatable (builder.builder) == 'hellbuilder',
					"Trying to use an invalid builder", 2)
			-- calling build with explicit builder field
			auto_builder = builder.builder
		end
		builder = auto_builder:extend (builder)
	end
	-- the new build
	local new = {
		__metatable = 'build',
		echo = builder.echo,
		input = builder.input,
		output = builder.output,
		cmd = subst (builder, 'cmd')
	}
	setmetatable (new, new)
	-- if no target specified, always add it to hell.builds
	if not _G.hell.target then
		table.insert (_G.hell.builds, new)
	end
	return new
end

--- The install function, which 
function install (builder)
	-- for install, always copy
	local new = {
		__metatable = 'install',
		echo = builder.echo,
		input = builder.input,
		output = builder.output,
		cmd = subst (copy:extend { input = builder.input }, 'cmd')
	}
	setmetatable (new, new)
	-- if no target specified, always add it to hell.installs
	if not _G.hell.target then
		table.insert (_G.hell.installs, new)
	end
	return new
end
