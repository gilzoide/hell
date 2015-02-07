import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
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
	mapM_ (Lua.pushstring l) args

	-- call main chunk
	ret <- Lua.pcall l (1 + length args) 0 0
	if ret  /= 0 then do
		{-Lua.getglobal2 l "debug.traceback"-}
		{-Lua.insert l (-2)-}
		{-Lua.call l 1 1-}
		err <- Lua.tostring l (-1)
		hPutStrLn stderr err
	else return ()

	Lua.close l
