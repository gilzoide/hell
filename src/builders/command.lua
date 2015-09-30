--- Run command Builder
command = Builder {
	cmd = '$command $args',
	-- command line arguments, to be put after command
	args = nil,
	-- force execution by setting output to something that'll never exist
	prepare_output = function () return '*!please/force/execution!*' end,
	prepare_input = function ()
		return ''
	end
}

command.help = [[Runs a customized command
This build is forced to run, as the output probably won't ever exist

NOTE: if this command build is a "run" target, set the "runTarget" field to
true, so that it'll only be executed on the "run" target]]
