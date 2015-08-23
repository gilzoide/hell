--- Run command Builder
command = Builder {
	cmd = 'please put your command here (including "./" if needed)',
	-- force execution by setting output to something that'll never exist
	prepare_output = function () return '/please/force/execution' end,
	prepare_input = function () return '' end
}

command.help = [[Runs a customized command
Note that this build forces to be run, as the output probably won't ever exist]]
