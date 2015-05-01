#pragma once

#include <lua.hpp>

extern "C" {
	/// Creates the Lua binding for hell's cpp stuff
	int luaopen_cppUtils (lua_State *L);
}
