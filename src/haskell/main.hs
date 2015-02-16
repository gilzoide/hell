import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Printf
import System.CPUTime
import Scripting.Lua as Lua
import Foreign.C.Types (CInt)

import HS_Utils

main = do
	l <- Lua.newstate
	Lua.openlibs l
	Lua.loadfile l "hell.lua"

	-- register utilities in lua
	registerHSUtils l

	-- pass arguments
	args <- getArgs
	pushList l args

	-- let's count the script run time
	start <- getCPUTime
	-- call main chunk
	ret <- Lua.pcall l 2 0 0
	end <- getCPUTime
	if ret  /= 0 then do
		{-Lua.getglobal2 l "debug.traceback"-}
		{-Lua.insert l (-2)-}
		{-Lua.call l 1 1-}
		err <- Lua.tostring l (-1)
		hPutStrLn stderr err
	else do
		let diff = (fromIntegral (end - start)) / (10^12)
		printf "Script execution time: %.3f s\n" (diff :: Double)

	Lua.close l

