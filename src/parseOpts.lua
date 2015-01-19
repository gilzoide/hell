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

-- All the argumments. Options will be extracted to the opts table,
-- while the others will be add in a var=val fashion (globally)
-- Note that 'opts' keep track of the options only in their short form
local args = {...}
local opts = {}
local var, val
local skip

--[[	Parse the opts	]]--
for i, arg in ipairs (args) do
	if skip then goto continue end
	skip = false
	-- long option
	local check = arg:match ("^%-%-(.*)")
	if check then
		var, val = check:match ("([%w_%-]+)=(.*)")
		if val and hell_options[var] then 
			if hell_options[var][3] then
				opts[hell_options[var][1]] = val
			else
				quit (var .. " option doesn't accept values, sorry", true)
			end
		elseif hell_options[check] then
			if not hell_options[check][3] then
				opts[hell_options[check][1]] = true
			else
				quit ("Value required for '" .. check .. "' long option.", true)
			end
		else
			quit ("Long option not recognized.\
Check `hell -H` for help.", true)
		end
		goto continue
	end
	-- short option
	check = arg:match ("^%-(.*)")
	if check then
		if hell_options[check] then
			-- with value, asks to skip the next
			if hell_options[check][3] then
				if args[i + 1] then
					opts[check] = args[i + 1]
					skip = true
				else
					quit ("Value required for '" .. check .. "' short option.", true)
				end
			else
				opts[check] = true
			end
		else
			quit ("Short option not recognized.\
Check `hell -H` for help.", true)
		end
		goto continue
	end
	-- variable attribution
	var, val = arg:match ("(.-)=(.+)")
	if not val then
		quit ('"' .. arg .. '" is not a valid option or variable attribution.\
Check `hell -H` for help.', true)
	else
		_ENV[var] = val
	end
	::continue::
end

--[[		Help!		]]--
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
Mandatory or optional arguments to long options are also mandatory or optional
for any corresponding short options.

Report bugs to <gilzoide@gmail.com>]])

	optionsString = table.concat (optionsString, '\n')

	help = help or optionsString
	quit (help)
elseif opts.V then
	quit ('hell 0.1.0')
end

return opts
