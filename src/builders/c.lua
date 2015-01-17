local util = require 'hellutils'

gcc = Builder {
	bin = 'gcc',
	flags = 'Wall',
	prepare_flags = util.curryPrefixEach (hell.os.flag),
	prepare_input = util.concat,
	cmd = '$bin $flags $input $output'
}
