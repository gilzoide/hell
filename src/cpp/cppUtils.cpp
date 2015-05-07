#include "cppUtils.hpp"
#include "BuildGraph.hpp"
#include "commonLib.hpp"

/// Get the OS name
int getOS (lua_State *L) {
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32)
    const char *osName = "windows";
#elif __APPLE__
    const char *osName = "darwin";
#else
    const char *osName = "unix";
#endif
    lua_pushstring (L, osName);
    return 1;
}


/// Glob, using the POSIX glob.h
// Lua params:
//     pattern: String with the pattern to be searched
int cppGlob (lua_State *L) {
	lua_newtable (L);
	return 1;
}


/// Call hellErrMsg
// Lua params:
//     msg: String with the message
int cppHellErrMsg (lua_State *L) {
	const char *msg = luaL_checkstring (L, 1);
	hellErrMsg (msg);
	return 0;
}


/// Call hellMsg
// Lua params:
//     msg: String with the message
//     verbose: Bool, should we write msg or not? (Skips only if false, not nil)
int cppHellMsg (lua_State *L) {
	const char *msg = luaL_checkstring (L, 1);
	hellMsg (msg);
	return 0;
}


/// Process the Builds/Installs
// Lua params:
//     BI: Table with the Builds
int processBI (lua_State *L) {
	BuildGraph G {L};
	G.ProcessBuilds ();
	return 0;
}


/// Sets the verbosity Level, expected to be executed only once
// Lua params:
//     verbose: - true -> Verbose;
//              - nil -> Default;
//              - false -> Silent
int cppSetVerbose (lua_State *L) {
	if (lua_isnil (L, 1)) {
		setVerbose (Verbosity::Default);
	}
	else if (lua_toboolean (L, 1)) {
		setVerbose (Verbosity::Verbose);
	}
	else {
		setVerbose (Verbosity::Silent);
	}

	return 0;
}


const struct luaL_Reg cppUtilsLib [] = {
	{"processBI", processBI},
	{"getOS", getOS},
	{"glob", cppGlob},
	{"hellErrMsg", cppHellErrMsg},
	{"hellMsg", cppHellMsg},
	{"setVerbose", cppSetVerbose},
    {NULL, NULL}
};


extern "C" {
	int luaopen_cppUtils (lua_State *L) {
		luaL_newlib (L, cppUtilsLib);
		return 1;
	}
} // end extern "C"
