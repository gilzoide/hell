module HS_Utils (registerHSUtils, pushList) where

import qualified Scripting.Lua as Lua
import Foreign.C.Types (CInt)
import Control.Monad (foldM)
-- What we will register
import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified System.Info as Info
import System.FilePath.Glob (glob)


registerHSUtils :: Lua.LuaState -> IO ()
registerHSUtils l = do
	Lua.newtable l
	pushFunToTable ("getOS", return Info.os :: IO String)
	pushFunToTable ("getArch", return Info.arch :: IO String)
	pushRawFunctions [("processBI", processBI),
		("glob", glob')]
	where
		pushRawFunctions = mapM_ pushRawFunToTable
		pushFunToTable (name, f) = do
			Lua.pushstring l name
			Lua.pushhsfunction l f
			Lua.settable l (-3)
		pushRawFunToTable (name, f) = do
			Lua.pushstring l name
			Lua.pushrawhsfunction l f
			Lua.settable l (-3)


-- | Haskell raw glob function to Lua. Expects a string, returns table of paths,
-- or nil plus error message
glob' :: Lua.LuaState -> IO CInt
glob' l = do
	valid <- Lua.isstring l (-1)
	if valid then do
		pattern <- Lua.tostring l (-1)
		Lua.pop l 1
		-- get list of relative paths from current directory
		currDir <- Dir.getCurrentDirectory
		fullpaths <- glob pattern
		let paths = map (Path.makeRelative currDir) fullpaths
		-- and push the whole list as a table
		pushList l paths
		return 1
	else do
		Lua.pop l 1
		Lua.pushnil l
		Lua.pushstring l "[glob] Can't glob against a non string pattern!"
		return 2


processBI :: Lua.LuaState -> IO CInt
processBI l = do
	-- first key
	Lua.pushnil l
	printCmdRec
	where
		printCmdRec = do
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

-- | Pushes a List into a LuaState
-- @note This function pushes a new table into the stack, even if list
-- passed is empty
pushList :: Lua.StackValue a => Lua.LuaState -> [a] -> IO Int
pushList l lst = do
	Lua.newtable l
	foldM pushListItem 1 lst
	where
		pushListItem :: Lua.StackValue a => Int -> a -> IO Int
		pushListItem n item = do
			Lua.push l n
			Lua.push l item
			Lua.settable l (-3)
			return $ n + 1
