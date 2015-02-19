import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Printf
import System.CPUTime

import HS_Utils
import Lua_Utils as Lua

main = do
	l <- Lua.newstate
	Lua.openlibs l

	-- push traceback before loading file, so pcall calls the main chunk right
	-- will be passed as the error handling function, so it's easier for
	-- debugging hell!
	Lua.getglobal2 l "debug.traceback"

	Lua.loadfile l "hell.lua"

	-- register utilities in lua
	registerHSUtils l

	-- pass arguments
	args <- getArgs
	Lua.pushList l args

	-- let's count the script run time
	start <- getCPUTime
	-- call main chunk
	ret <- Lua.pcall l 2 0 (-4)
	end <- getCPUTime
	if ret  /= 0 then do
		err <- Lua.tostring l (-1)
		hPutStrLn stderr err
	else do
		let diff = (fromIntegral (end - start)) / (10^12)
		printf "Script execution time: %.3f s\n" (diff :: Double)

	Lua.close l
