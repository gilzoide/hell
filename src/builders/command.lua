--- Run command Builder
command = Builder {
	cmd = 'please put your command here (including "./" if needed)',
	-- if this command is a "run" target, set `run' to true, and it'll only be
	-- executed when calling the "run" target (which is common practice to have)
	runTarget = nil,
	-- force execution by setting output to something that'll never exist
	prepare_output = function () return '*!please/force/execution!*' end,
	prepare_input = function (_, b)
		if b.runTarget and utils.getOption 'target' ~= 'run' then
			b.cmd = ''
		end
		return ''
	end
}

command.help = [[Runs a customized command
This build is forced to run, as the output probably won't ever exist

NOTE: if this command build is a "run" target, set the "runTarget" field to
true, so that it'll only be executed on the "run" target]]
