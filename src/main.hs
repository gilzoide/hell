import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Scripting.Lua

main = do
	l <- newstate
	openlibs l
	loadfile l "hell.lua"
	args <- getArgs
	mapM_ (pushstring l) args
	ret <- pcall l (length args) 0 0
	if ret  /= 0 then do
		err <- tostring l (-1)
		hPutStrLn stderr err
	else return ()

	close l
