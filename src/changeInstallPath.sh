#!/bin/sh

if [ ! $1 ]; then
	echo "Usage: changeInstallPath.sh PATH"
	exit
fi

sed -i "/^local hellInstallPath/ c local hellInstallPath = \'$1/lib/hell\'" lua/hell.lua
echo Hell install path changed to $1 successfully!

exit
