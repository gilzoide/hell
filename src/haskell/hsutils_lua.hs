module HS_Utils (registerHSUtils) where

import qualified Scripting.Lua as Lua
import Foreign.C.Types (CInt)
-- What we will register
import qualified System.Directory as Dir
import qualified System.Info as Info

registerHSUtils :: Lua.LuaState -> IO ()
registerHSUtils l = do
	Lua.newtable l
	pushFunctions [
		("getOS", (return Info.os :: IO String)),
		("getArch", return Info.arch :: IO String)]
	pushRawFunctions [
		("processBuilds", processBuilds),
		("processInstalls", processInstalls)]
	where
		pushFunctions = mapM_ pushFunToTable
		pushRawFunctions = mapM_ pushRawFunToTable
		pushFunToTable (name, f) = do
			Lua.pushstring l name
			Lua.pushhsfunction l f
			Lua.settable l (-3)
		pushRawFunToTable (name, f) = do
			Lua.pushstring l name
			Lua.pushrawhsfunction l f
			Lua.settable l (-3)


processBuilds :: Lua.LuaState -> IO CInt
processBuilds l = do
	-- first key
	Lua.pushnil l
	printCmdRec
	where
		printCmdRec = do
			size <- Lua.gettop l
			{-putStrLn $ " size: " ++ show size-}
			theresMore <- Lua.next l (-2)
			if theresMore then do
				-- process dependencies first
				Lua.getfield l (-1) "deps"
				Lua.pushnil l
				printCmdRec
				-- and now back to our build
				Lua.getfield l (-1) "cmd"
				cmd <- Lua.tostring l (-1)
				putStrLn cmd
				-- pop `cmd' and the current build, leaves the key for `next'
				Lua.pop l 2
				printCmdRec
			else do
				Lua.pop l 1
				return 0

processInstalls :: Lua.LuaState -> IO CInt
processInstalls l = do
	return 0
