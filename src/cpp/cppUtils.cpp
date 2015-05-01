#include "cppUtils.hpp"
#include "processBI.hpp"

int getOS (lua_State *L) {
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) && !defined(__CYGWIN__)
    const char * osName = "windows";
#elif __APPLE__
    const char * osName = "darwin";
#else
    const char * osName = "unix";
#endif
    lua_pushstring (L, osName);
    return 1;
}


int cppGlob (lua_State *L) {
	lua_newtable (L);
	return 1;
}


const struct luaL_Reg cppUtilsLib [] = {
	{"processBI", processBI},
	{"getOS", getOS},
	{"glob", cppGlob},
    {NULL, NULL}
};


extern "C" {
	int luaopen_cppUtils (lua_State *L) {
		luaL_newlib (L, cppUtilsLib);
		return 1;
	}
} // end extern "C"
