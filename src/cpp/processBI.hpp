#pragma once

#include <lua.hpp>

/**
 * Process Hell's builds
 *
 * In Lua, it's arguments are a table with the wanted builds.
 */
int processBI (lua_State *L);
