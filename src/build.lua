--- Auxiliary function for substituting the fields in a command
--
-- @note When a field from t is nil, it's entry is substituted with ''
-- (just like shell would do).
--
-- @param[in] t The table with the fields
-- @param[in] cmd The command to be formed
--
-- @return A string with the right command
function subCmd (builder)
	assert (type (builder.cmd) == 'string', "[subCmd] Can't substitute command: builder.cmd isn't a string")

	-- build the command substituting anything that starts with a '$'
	-- (unless it's escaped with another '$')
	local function sub (capture)
		if capture:sub (1, 1) == '$' then
			return capture
		else
			-- call the field's 'prepare_' function, if it exists, or return the
			-- field, or an empty string
			local field, prepare = builder[capture], builder['prepare_' .. capture]
			return prepare and prepare (field) or field or ''
		end
	end

	return builder.cmd:gsub ('$([$%w]+)', sub)
end
