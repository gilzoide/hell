local utils = hell.utils

flex = Builder {
	bin = 'flex',
	cmd = '$bin -o $output $input',
	prepare_output = function (o, i)
		return o or utils.changeExtension ('c', i)
	end,
	help = [[
Flex scanner generator
By default, it generates a .c file with the same basename as its .l file]]
}
-- Set the default '.l' file Builder as flex
l = flex
