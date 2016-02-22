#include "cppUtils.hpp"
#include "BuildGraph.hpp"
#include "commonLib.hpp"

#include <cstring>
#include <sys/stat.h>
#include <unistd.h>
#include <glob.h>
#include <chrono>
#include <iomanip>

/// Get the OS name
// Lua return: OS name
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


/// Get the OS architecture
// Lua return: OS architecture
int getArch (lua_State *L) {
#if defined(__i386__) || defined(_M_IX86) || defined(_M_I86)
    const char *archName = "x86";
#elif defined(__x86_64__) || defined(_M_X64)
    const char *archName = "x86_64";
#elif defined(__ia64__) || defined(_M_IA64)
	const char *archName = "ia64";
#elif defined(__ppc__) || defined(_M_PPC)
	const char *archName = "ppc";
#elif __aarch64__
	const char *archName = "arm64";
#elif defined(__arm__) || defined(_M_ARM)
	const char *archName = "arm";
#elif __mips__
	const char *archName = "mips";
#else
    const char *archName = "unknown";
#endif
    lua_pushstring (L, archName);
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
int cppHellMsg (lua_State *L) {
	const char *msg = luaL_checkstring (L, 1);
	hellMsg (msg);
	return 0;
}


// some definitions so we can track down the time spent
using namespace std::chrono;
using clk = steady_clock;
/// The timer starting point
clk::time_point start = clk::now ();

/// Start the timer
void startTimer () {
    start = clk::now ();
}

/// startTimer Lua wrapper
int cppStartTimer (lua_State *L) {
	startTimer ();
	return 0;
}

/// Shows time elapsed between `start' and now, in seconds (3 decimal places)
void showElapsedTime (const char *job) {
    if (Opts::getInstance ().get_timer ()) {
		const auto dt = duration_cast<milliseconds> (clk::now () - start)
				.count () / 1000.0;
		ostringstream str;
		str << job << " time: " << fixed << setprecision (3) << dt << "s";
		hellMsg (str.str ());
	}
}

/// showElapsedTime Lua wrapper
// Lua params:
//     jobName: the job's name (what job the timer was measuring)
int cppShowElapsedTime (lua_State *L) {
	const char *jobName = luaL_checkstring (L, 1);
	showElapsedTime (jobName);

	return 0;
}


/// Process the Builds/Installs
// Lua params:
//     BI: Table with the Builds
int processBI (lua_State *L) {
	// starting clock, so we can measure the elapsed time
	startTimer ();

	BuildGraph G {L};
	G.processBuilds ();

	// if asked to show the time elapsed, let'sa do it!
	showElapsedTime ("Build processing");

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
	int flags;
#ifdef __GNUC__
	// GNU extensions: ~ and {} patterns
	flags = GLOB_TILDE | GLOB_BRACE;
#else
	flags = 0;
#endif

	glob (pattern, flags, nullptr, &globbuf);

	// creates the table and puts the matches inside it
	lua_newtable (L);
	for (unsigned int i = 0; i < globbuf.gl_pathc; i++) {
		// i + 1, because Lua
		lua_pushinteger (L, i + 1);
		lua_pushstring (L, globbuf.gl_pathv[i]);
		lua_settable (L, -3);
	}

	globfree (&globbuf);

	return 1;
}


/// Prefix string with prefix, if not already done
// Lua params:
//     prefix: String with the prefix
//     str: String to be prefixed if needed
// Lua return: final string
int cppLazyPrefix (lua_State *L) {
	size_t size;
	const char *prefix = luaL_checklstring (L, 1, &size);
	const char *str = luaL_checkstring (L, 2);

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


/// Set the opts important to C++, instancing the singleton Opts class
// Lua params:
//     opts: Table with the options
// Lua return:
//     bool: Is numJobs a valid value?
int cppSetOpts (lua_State *L) {
    lua_getfield (L, 1, "j");
    int numJobs = 1;
    if (!lua_isnil (L, -1)) {
        numJobs = lua_tointeger (L, -1);
    }

    Verbosity verbose;
    lua_getfield (L, 1, "v");
    // opts.v -> verbose
    if (!lua_isnil (L, -1)) {
        verbose = Verbosity::Verbose;
    }
    else {
        lua_getfield (L, 1, "s");
        // opts.s -> silent
        if (!lua_isnil (L, -1)) {
            verbose = Verbosity::Silent;
        }
        // none -> default
        else {
            verbose = Verbosity::Default;
        }

        // pops 's', as 'v' will always be popped anyway
        lua_pop (L, 1);
    }

    lua_getfield (L, 1, "n");
    bool dryRun = !lua_isnil (L, -1);

    lua_getfield (L, 1, "F");
    bool force = !lua_isnil (L, -1);

    lua_getfield (L, 1, "t");
    bool timer = !lua_isnil (L, -1);

    lua_getfield (L, 1, "C");
    bool C = !lua_isnil (L, -1);

    lua_getfield (L, 1, "d");
    bool depTree = !lua_isnil (L, -1);
    
    bool isNumJobsValid = Opts::getInstance ().setOpts (numJobs, verbose,
			dryRun, force, timer, C, depTree);
    lua_pop (L, 7);

	lua_pushboolean (L, isNumJobsValid);
    return 1;
}


/// Creates directory, if it doesn't already exists
// Lua params:
//     dirName: String with the directory name
int cppCreateDirIfNeeded (lua_State *L) {
	const char *dirName = luaL_checkstring (L, 1);

	// run mkdir, giving an error if error different from EEXIST
	if (mkdir (dirName, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) < 0 &&
			errno != EEXIST) {
		ostringstream os;
		os << "can't create directory " << dirName << ": " 
				<< strerror (errno);
		hellErrMsg (os.str ());
	}

	return 0;
}


/// Shell's `cd`
// Lua params:
//     dirName: String with the directory name
// Lua return:
//     true if everything all right, nil + msg otherwise
int cppChDir (lua_State *L) {
	const char *dirName = luaL_checkstring (L, 1);
	int ret = chdir (dirName);

	if (ret) {
		lua_pushnil (L);
		lua_pushstring (L, "CD failed!");
		return 2;
	}
	else {
		lua_pushboolean (L, 1);
		return 1;
	}
}


/// Get current working directory
// Lua return:
//     String with current working directory's absolute path
int cppGetCwd (lua_State *L) {
#define BUFSIZE 100
	char dirName[BUFSIZE];
	getcwd (dirName, BUFSIZE);

	lua_pushstring (L, dirName);
	return 1;
#undef BUFSIZE
}


const struct luaL_Reg cppUtilsLib [] = {
	{"processBI", processBI},
	{"getOS", getOS},
	{"getArch", getArch},
	{"glob", cppGlob},
	{"lazyPrefix", cppLazyPrefix},
	{"hellErrMsg", cppHellErrMsg},
	{"hellMsg", cppHellMsg},
	{"createDirIfNeeded", cppCreateDirIfNeeded},
	{"setOpts", cppSetOpts},
	{"chdir", cppChDir},
	{"getcwd", cppGetCwd},
	{"startTimer", cppStartTimer},
	{"showElapsedTime", cppShowElapsedTime},
    {NULL, NULL}
};


extern "C" {
	int luaopen_cppUtils (lua_State *L) {
		luaL_newlib (L, cppUtilsLib);
		return 1;
	}
} // end extern "C"
