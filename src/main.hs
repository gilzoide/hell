import System.Environment (getArgs)
import Scripting.Lua

main = do
	l <- newstate
	openlibs l
	loadfile l "hell.lua"
	args <- getArgs
	mapM_ (pushstring l) args
	call l (length args) 0

	close l
