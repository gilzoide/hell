#include "cppUtils.hpp"
#include "BuildGraph.hpp"

int getOS (lua_State *L) {
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32)
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


int cppHellErrMsg (lua_State *L) {
	const char *msg = luaL_checkstring (L, 1);
	HellErrMsg (msg);
	return 0;
}


int processBI (lua_State *L) {
	BuildGraph G {L};
	G.ProcessBuilds ();
	return 0;
}


const struct luaL_Reg cppUtilsLib [] = {
	{"processBI", processBI},
	{"getOS", getOS},
	{"glob", cppGlob},
	{"HellErrMsg", cppHellErrMsg},
    {NULL, NULL}
};


extern "C" {
	int luaopen_cppUtils (lua_State *L) {
		luaL_newlib (L, cppUtilsLib);
		return 1;
	}
} // end extern "C"
