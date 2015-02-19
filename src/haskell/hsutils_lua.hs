module HS_Utils (registerHSUtils) where

import qualified Lua_Utils as Lua
import Foreign.C.Types (CInt)
-- What we will register
import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified System.Info as Info
import System.FilePath.Glob (glob)


-- | Register what we need in the LuaState, in a new table
registerHSUtils :: Lua.LuaState -> IO ()
registerHSUtils l = do
	Lua.newtable l
	Lua.pushFunctions l [("getOS", return Info.os :: IO String),
		("getArch", return Info.arch :: IO String)]
	Lua.pushRawFunctions l [("processBI", processBI),
		("glob", glob')]


-- | Haskell raw glob function to Lua. Expects a string, returns table of paths,
-- or nil plus error message
glob' :: Lua.LuaCFunction
glob' l = do
	valid <- Lua.isstring l (-2)
	if valid then do
		pattern <- Lua.tostring l (-2)
		baseDir <- ((Lua.tostring l (-1)) >>= Dir.canonicalizePath)
		Lua.pop l 2
		-- get list of paths, and make them relative from `baseDir'
		fullpaths <- glob (baseDir Path.</> pattern)
		if fullpaths == [] then do
			-- no matches: push nil
			Lua.pushnil l
			return 1
		else do
			let paths = map (Path.makeRelative baseDir) fullpaths
			-- push the whole list as a table
			Lua.pushList l paths
			return 1
	else do
		Lua.pop l 2
		Lua.pushnil l
		Lua.pushstring l "[glob] Can't glob against a non string pattern!"
		return 2


-- | Process builds/installs. Expects a table passed as argument (from Lua)
-- Builds the z
processBI :: Lua.LuaCFunction
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
