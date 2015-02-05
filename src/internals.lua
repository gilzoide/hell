--- @file internals.lua 
-- Functions to be used by hell only (suck that mango, user!)

local t = {}

--- Prints a message from hell execution
function t.hellMsg (msg)
	if t.verbose ~= false then
		print ('hell: ' .. msg)
	end
end


--- Quits the program with a message, and sign possible error
function t.quit (msg, was_error)
	io.stderr:write ('hell: ' .. msg .. '\n')
	os.exit (was_error and 0 or 1, true)
end


--- Assertion with custom quit handler (function quit)
--
-- @param cond The condition to be checked. If false, quit handler will
--  be called.
-- @param msg The message to be displayed. There's no default, please
--  provide one.
-- @param level Debug level, for showing where the problem happend.
--  Set the level just like you would in debug.getinfo, as assert_quit already
--  increments itself in the level.
--
-- @return If condition is true, it's returned (just like assert does)
function t.assert_quit (cond, msg, level)
	if not cond then
		-- maybe we want to trace where the problem happened, so...
		if level then
			-- need to set level+1, so we don't count assert_quit itself
			local script = debug.getinfo (level + 1)
			msg = script.short_src .. ':' .. script.currentline .. ': ' .. msg
		end
		t.quit (msg, true)
	end

	return cond
end


--- A stack for the paths, which will be used when sourcing a hellfire,
-- for `build' and `install' to know where to look for inputs.
-- First path is where we are now
t.path = {}

function t.getPath ()
	local dir = table.concat (t.path, hell.os.dir_sep, 1)
	if dir ~= '' then
		dir = dir .. hell.os.dir_sep
	end

	return dir
end

return t