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

-- all the hell options
local hell_options = {
	{'f', 'file', 'FILE', "Specify build script name. Defaults: hellbuild or hellfire"},
	{'j', 'jobs', 'N', "Allow N jobs at once. Must be between 1~100"},
	{'s', 'silent', nil, "Suppress stdout output"},
	{'v', 'verbose', nil, "Print all the commands executed, surpassing the build's 'echo' field"},
	{'l', 'list-targets', nil, "List all possible targets"},
	{'t', 'timer', nil, "Show builds/installs' process time"},
	{'h', 'help', nil, "Show script's custom help, or this one"},
	{'H', 'help-options', nil, "Give this help list"},
	{'n', 'no-exec', nil, "Don't execute commands, just print them"},
	{'V', 'version', nil, "Print program version"},
	{'c', 'ignore-cyclic-dependency', nil, [[Ignores cyclic dependencies, signing the following contract:
"I am aware that my build may have cyclic dependencies, and declare hell's developer innocent if my PC burns"]]},
}

-- associate each short and long flags with their table
for _, v in ipairs (hell_options) do
	hell_options[v[1]] = v
	hell_options[v[2]] = v
end

-- All the arguments. Options will be extracted to the opts table,
-- while the others will be add in a var=val fashion (globally)
-- Note that 'opts' keep track of the options only in their short form
local args = {...}
local opts = {}
local skip

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
		if val and hell_options[var] then 
			if hell_options[var][3] then
				opts[hell_options[var][1]] = val
			else
				int.quit (var .. " option doesn't accept values, sorry", true)
			end
		elseif hell_options[long] then
			if not hell_options[long][3] then
				opts[hell_options[long][1]] = true
			else
				int.quit ("Value required for '--" .. long .. "' long option.", true)
			end
		else
			int.quit ("Long option not recognized.\
Check `hell -H` for help.", true)
		end
	-- short option
	elseif short then
		if hell_options[short] then
			-- with value, asks to skip the next
			if hell_options[short][3] then
				if args[i + 1] then
					opts[short] = args[i + 1]
					skip = true
				else
					int.quit ("Value required for '-" .. short .. "' short option.", true)
				end
			else
				opts[short] = true
			end
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


--[[		Now that we parsed the options, make them active!		]]--
opts.command = opts.command or 'build'

-- set the opts important to C++ in the singleton `Opts' class
int.assert_quit (int.cpp.setOpts (opts),
		'Invalid number of jobs: must be between 1 and 100')

-- Version
if opts.V then
	int.quit ('hell 0.1.0')
end

-- Help! Should be called after sourcing the scripts, as it may override hell.help
function hellp ()
	local optionsString, help
	if opts.h then
		help = hell.help
		opts.H = true
	end
	if opts.H then
		optionsString = {
			"Usage: hell [OPTIONS...] [TARGET] { build | clean | install | uninstall } [var=value]\
\
Hell options:"
		}
		for _, v in ipairs (hell_options) do
			local short = v[1] .. (v[3] and ' ' .. v[3] or '')
			local long = v[2] .. (v[3] and '=' .. v[3] or '') 
			table.insert (optionsString, string.format ('%-35s %s', '    -' .. short .. ', --' .. long, v[4]))
		end
		table.insert (optionsString, [[ 
All arguments to short options and their long counterpart are mandatory.

Any bugs should be reported to <gilzoide@gmail.com>]])

		optionsString = table.concat (optionsString, '\n')

		help = (help and help .. '\n\nUse `hell -H` for more help.') or optionsString
		int.quit ('help:\n' .. help)
	end
end

if opts.H then
	hellp ()
end

return opts
