import Scripting.Lua
import System.Environment

main = do
	l <- newstate
	openlibs l
	args <- getArgs
	{-mapM putStrLn args-}
	{-mapM (pushstring l) args-}
	loadfile l "hell.lua"
	call l 0 0
	close l
