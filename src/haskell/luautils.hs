module Lua_Utils (module Lua_Utils, module Scripting.Lua) where

import Scripting.Lua
import Control.Monad (foldM_)

-- | Push a list of functions to LuaState, with their respective names
pushFunctions :: LuaImport a => LuaState -> [(String, a)] -> IO ()
pushFunctions l = mapM_ (pushFunToTable l)


-- | Push a list of raw functions to LuaState, with their respective names
pushRawFunctions :: LuaState -> [(String, LuaCFunction)] -> IO ()
pushRawFunctions l = mapM_ (pushRawFunToTable l)


-- | Push a function to LuaState, with it's given name
pushFunToTable :: LuaImport a => LuaState -> (String, a) -> IO ()
pushFunToTable l (name, f) = do
	pushstring l name
	pushhsfunction l f
	settable l (-3)


-- | Push a raw function to LuaState, with it's given name
pushRawFunToTable :: LuaState -> (String, LuaCFunction) -> IO ()
pushRawFunToTable l (name, f) = do
	pushstring l name
	pushrawhsfunction l f
	settable l (-3)


-- | Pushes a List into a LuaState
-- @note This function pushes a new table into the stack, even if list
-- passed is empty
pushList :: StackValue a => LuaState -> [a] -> IO ()
pushList l lst = do
	newtable l
	foldM_ pushListItem 1 lst
	where
		pushListItem :: StackValue a => Int -> a -> IO Int
		pushListItem n item = do
			push l n
			push l item
			settable l (-3)
			return $ n + 1
