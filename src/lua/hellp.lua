--- @file hellp.lua
-- hell help definitions

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

local hellp = {}

local int = require 'internals'
local util = hell.utils

-- all the hell options
hellp.hell_options = {
	{'f', 'file', 'FILE', "Specify build script name. Defaults: hellbuild or hellfire"},
	{'j', 'jobs', 'N', "Allow N jobs at once. Must be between 1~100"},
	{'s', 'silent', nil, "Suppress stdout output"},
	{'v', 'verbose', nil, "Print all the commands executed, surpassing the build's 'echo' field"},
	{'l', 'list-targets', nil, "List all available targets"},
	{'t', 'timer', nil, "Show script read and builds/installs' process time"},
	{'h', 'help', nil, "Show script's custom help, or this one"},
	{'H', 'help-options', nil, "Give this help list"},
	{'u', 'usage', nil, "Show usage"},
	{'hb', 'help-builder', 'BUILDER', "Show Builder's hellp"},
	{'d', 'dependency-trees', nil, "Show target's dependency trees"},
	{'n', 'no-exec', nil, "Don't execute commands, just print them"},
	{'c', 'clean-outdir', nil, "Clean hell.outdir (if defined), instead of each output separately"},
	{'V', 'version', nil, "Print program version"},
	{'C', 'ignore-cyclic-dependency', nil, [[Ignores cyclic dependencies, signing the following contract:
"I am aware that my build may have cyclic dependencies, and declare hell's developer innocent if my PC burns"]]},
}


function hellp.getBuilderHellp (builderName)
	local builder = util.getNestedField (_ENV, builderName)
	local msg

	if builder then
		msg = '"' .. builderName .. "\" builder\n" .. 
				(builder.help or '') .. 
				"\ncmd: " .. builder.cmd
	else
		msg = '"' .. builderName .. "\" builder doesn't exist"
	end
	return msg
end

hellp.usage = "Usage: hell [OPTIONS...] [TARGET] { build | clean | install | uninstall } [var=value]"

-- Make hellp table callable
setmetatable (hellp, {__call = 
	function (self, msg)
		local optionsString

		if not msg then
			optionsString = {
				"Welcome to Hellp (Hell's help)\n",
				self.usage,
				"\nOptions:"
			}
			for _, v in ipairs (self.hell_options) do
				local short = v[1] .. (v[3] and ' ' .. v[3] or '')
				local long = v[2] .. (v[3] and '=' .. v[3] or '') 
				table.insert (optionsString, string.format ('%-40s %s', '    -' .. short .. ', --' .. long, v[4]))
			end
			table.insert (optionsString, [[ 
All arguments to short options and their long counterpart are mandatory.

Any bugs should be reported to <gilzoide@gmail.com>]])

			optionsString = table.concat (optionsString, '\n')
		end

		optionsString = msg and msg .. '\n\nUse `hell -H` for more help.' or optionsString
		int.quit ('help:\n' .. optionsString)
	end
})

return hellp
