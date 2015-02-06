module HS_Utils (registerHSUtils) where

import Scripting.Lua
import Foreign.C.Types (CInt)
-- What we will register
import qualified System.Directory as Dir
import qualified System.Info as Info

registerHSUtils :: LuaState -> IO ()
registerHSUtils l = do
	registerhsfunction l "getOS" (return Info.os :: IO String)
	registerhsfunction l "getArch" (return Info.arch :: IO String)
	return ()
