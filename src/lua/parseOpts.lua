--- @file parseOpts.lua
-- Options, right?

--[[
-- Copyright (C) 2015 Gil Barbosa Reis
-- This file is part of Hell.
--
-- Hell is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Hell is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Hell.  If not, see <http://www.gnu.org/licenses/>.
--]]

local int = require 'internals'
local hellp = require 'hellp'

-- associate each short and long flags with their table
for _, v in ipairs (hellp.hell_options) do
	hellp.hell_options[v[1]] = v
	hellp.hell_options[v[2]] = v
end

-- Options will be extracted to the opts table,
-- while the others will be add in a var=val fashion (globally)
-- Note that 'opts' keep track of the options only in their short form
local opts = {}
local skip

-- Save the `arg' global variable, and clean it, so that it holds only
-- arguments after the '--' short option
local args = _ENV['arg']
_ENV['arg'] = {}

-- hue =P
if #args == 1 and args[1] == 'bells' then
	int.quit ([[It's "hells bells", poser (don't come aliasing on me, troos don't cheat) \m/]], true)
end

--[[	Parse the opts	]]--
for i, arg in ipairs (args) do
	local long = arg:match ("^%-%-(.+)")
	local short = arg:match ("^%-(.+)")
	local var, val = arg:match ("(.-)=(.+)")

	-- skip flag: for options that need values
	if skip then
		skip = false
	-- long option
	elseif long then
		var, val = long:match ("([%w_%-]+)=(.+)")
		if val and hellp.hell_options[var] then 
			if hellp.hell_options[var][3] then
				opts[hellp.hell_options[var][1]] = val
			else
				int.quit (var .. " option doesn't accept values, sorry", true)
			end
		elseif hellp.hell_options[long] then
			if not hellp.hell_options[long][3] then
				opts[hellp.hell_options[long][1]] = true
			else
				int.quit ("Value required for '--" .. long .. "' long option.", true)
			end
		else
			int.quit ("Long option not recognized.\
Check `hell -H` for help.", true)
		end
	-- short option
	elseif short then
		if hellp.hell_options[short] then
			-- with value, asks to skip the next
			if hellp.hell_options[short][3] then
				if args[i + 1] then
					opts[short] = args[i + 1]
					skip = true
				else
					int.quit ("Value required for '-" .. short .. "' short option.", true)
				end
			else
				opts[short] = true
			end
		-- stop reading command line options, setting `arg' to the right value
		elseif short == '-' then
			-- use _ENV['arg'] so there's no clash with our local `arg' variable
			_ENV['arg'] = table.pack (select (i + 1, table.unpack (args)))
			break
		else
			int.quit ("Short option not recognized.\
Check `hell -H` for help.", true)
		end
	-- variable attribution
	elseif val then
		_ENV[var] = val
	-- or the command (must be one the valid ones)
	elseif (' build clean install uninstall '):match ('%s+' .. arg .. '%s+') then
		opts.command = arg
	-- or target
	elseif not opts.target then
		opts.target = arg
	else
		int.quit ('"' .. arg .. '" is not a valid option or variable attribution.\
Check `hell -H` for help.', true)
	end
end

-- ensure arg[0] is 'hell', not 'lua'
arg[0] = 'hell'

if opts.u then
	int.quit (hellp.usage)
elseif opts.H then
	hellp ()
-- Version
elseif opts.V then
	int.quit ('version ' .. int.version)
end



-- if not interested in any help nor target list, use a valid command
if not (opts.h or opts.hb or opts.l) then
	opts.command = opts.command or 'build'
else
	opts.command = nil
end

-- set the opts important to C++ in the singleton `Opts' class
int.assert_quit (int.cpp.setOpts (opts),
		'Invalid number of jobs: must be between 1 and 100')

-- set internals' opts, so all across hell we have access to it =]
int.opts = opts
