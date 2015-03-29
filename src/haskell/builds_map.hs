module Builds_Map where

import qualified Data.Map as Map
import qualified Lua_Utils as Lua

data Build = Build { input :: [String],	-- input list
					 output :: String,	-- output name
					 deps :: [String],	-- list of dependencies, by output name
					 cmd :: String,		-- command to be run
					 processed :: Bool	-- have it been processed already?
					} deriving Show

makeMap :: Lua.LuaState -> IO (Map.Map String Build)
makeMap l = do
	Lua.pushnil l
	lst <- getList []
	let mapa = Map.fromList lst
	putStrLn $ Map.showTree mapa
	return mapa
	where
		getList lst = do
			theresMore <- Lua.next l (-2)
			if theresMore then do
				-- process dependencies first
				Lua.getfield l (-1) "deps"
				Lua.pushnil l
				deps <- getList []
				-- and now back to our build
				Lua.getfield l (-1) "cmd"
				cmd <- Lua.tostring l (-1)
				Lua.getfield l (-2) "output"
				output <- Lua.tostring l (-1)
				Lua.getfield l (-3) "input"
				input <- Lua.getStringList l
				-- pop `cmd', output, input and the current build, leaves the key for `next'
				Lua.pop l 4
				getList $ (output, Build input output (input ++ getOutput deps) cmd False) : (lst ++ deps)
			else do
				Lua.pop l 1
				return lst
		getOutput = map (output . snd)
