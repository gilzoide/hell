#!/bin/sh

if ! [ "$1" ]
then
	echo "please, insert example name"
	exit
fi

# buffers file name (argument 1) and discard it from argv
arg1=$1
shift

cmd="hell -f $arg1/hellfire -n $@"

# run hell with the example at the '$1' directory, dryrun (so we don't need to
# have valid source files =]), and with any other arguments given
echo "[runExample] running \`$cmd\`"
echo

$cmd
