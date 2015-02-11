--- @file parseOpts.lua
-- Options, right?

local int = require 'internals'

-- all the hell options
local hell_options = {
	{'f', 'file', 'FILE', "Specify build script name. Defaults: hellbuild or hellfire"},
	{'j', 'jobs', 'N', "Allow N jobs at once"},
	{'s', 'silent', nil, "Suppress stdout output"},
	{'v', 'verbose', nil, "Print all the commands executed, surpassing the build's 'echo' field"},
	{'l', 'list-targets', nil, "List all possible targets"},
	{'t', 'tree', nil, "Show the target's dependency tree"},
	{'h', 'help', nil, "Show script's custom help, or this one"},
	{'H', 'help-options', nil, "Give this help list"},
	{'V', 'version', nil, "Print program version"}
}

-- associate each short and long flags with their table
for _, v in ipairs (hell_options) do
	hell_options[v[1]] = v
	hell_options[v[2]] = v
end

-- All the arguments. Options will be extracted to the opts table,
-- while the others will be add in a var=val fashion (globally)
-- Note that 'opts' keep track of the options only in their short form
local args = ...
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
		getfenv ()[var] = val
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
-- Verbose (if -v, true; if -s, false; else, nil)
int.verbose = opts.v or opts.s and false

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
			"Usage: hell [OPTIONS...] [TARGET] { build | clean | install | uninstall }\
\
Hell options:"
		}
		for _, v in ipairs (hell_options) do
			local short = v[1] .. (v[3] and ' ' .. v[3] or '')
			local long = v[2] .. (v[3] and '=' .. v[3] or '') 
			table.insert (optionsString, string.format ('%-30s %s', '  -' .. short .. ', --' .. long, v[4]))
		end
		table.insert (optionsString, [[ 
All arguments to short options and their long counterpart are mandatory.

Report bugs to <gilzoide@gmail.com>]])

		optionsString = table.concat (optionsString, '\n')

		help = (help and help .. '\n\nUse `hell -H` for more help.')or optionsString
		int.quit ('help:\n' .. help)
	end
end

if opts.H then
	hellp ()
end

return opts
