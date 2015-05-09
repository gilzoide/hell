#include "cppUtils.hpp"
#include "BuildGraph.hpp"
#include "commonLib.hpp"

#include <cstring>
#include <glob.h>

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



/// Makes a POSIX glob pattern matching
// Lua params:
//     pattern: String with the pattern to be matched
// Lua return: Table with matched filepaths
int cppGlob (lua_State *L) {
	const char *pattern = luaL_checkstring (L, 1);

	// glob buffer
	glob_t globbuf;
	glob (pattern, 0, nullptr, &globbuf);

	// creates the table and puts the matches inside it
	lua_newtable (L);
	for (unsigned int i = 0; i < globbuf.gl_pathc; i++) {
		lua_pushinteger (L, i + 1);
		lua_pushstring (L, globbuf.gl_pathv[i]);
		lua_settable (L, -3);
	}

	globfree (&globbuf);

	return 1;
}


/// Prefix string with prefix, if not already done
// Lua params:
//     str: String to be prefixed if needed
//     prefix: String with the prefix
// Lua return: final string
int cppLazyPrefix (lua_State *L) {
	const char *str = luaL_checkstring (L, 1);
	size_t size;
	const char *prefix = luaL_checklstring (L, 2, &size);

	// no match, so we need to prefix str
	if (strncmp (prefix, str, size)) {
		lua_pushlstring (L, prefix, size);
		lua_pushstring (L, str);
		lua_concat (L, 2);
	}
	// first 'size' chars match: already prefixed!
	else {
		lua_pushstring (L, str);
	}

	return 1;
}


const struct luaL_Reg cppUtilsLib [] = {
	{"processBI", processBI},
	{"getOS", getOS},
	{"glob", cppGlob},
	{"lazyPrefix", cppLazyPrefix},
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
