#include "build.hpp"


Build::Build (lua_State *L) {
	lua_getfield (L, -1, "input");
	lua_len (L, -1);
	input = new std::string [luaL_checkint (L, -1)];
	lua_pop (L, 1);


	lua_getfield (L, -2, "output");
	output = luaL_checkstring (L, -1);

	lua_getfield (L, -3, "cmd");
	cmd = luaL_checkstring (L, -1);

	// echo is optional
	lua_getfield (L, -4, "echo");
	if (!lua_isnil (L, -1)) {
		echo = luaL_checkstring (L, -1);
	}

	// clean stack
	lua_pop (L, 4);
}


Build::~Build () {
	delete[] input;
	delete[] deps;
}
