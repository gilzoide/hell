/** @file build.hpp
 */
#pragma once

#include <lua.hpp>
#include <iostream>

#include <forward_list>
#include <vector>
#include <map>

// Forward declaration, for Map to work
class Build;

using namespace std;
using Map = map<const void *, Build *>;

/**
 * Hell's builds, mapped exactly as Lua's
 */
class Build {
public:
	/**
	 * Ctor, from Lua's "build" metatable
	 *
	 * @note The table is expected to be on top of the stack, which
	 *  is not checked
	 * @note The "build" metatable is also not checked
	 *
	 * @param[in] L The using lua_State
	 */
	Build (lua_State *L, Map & AllBuilds);

	/**
	 * A simple printing function
	 */
	void print ();

private:
	/// Echo field, line to be echoed when running command; optional
	string echo;
	/// Cmd field, the command to be run
	string cmd;
	/// Output field, the build's output name
	string output;
	/**
	 * Input vector, the build's inputs (which are also dependencies)
	 *
	 * @note Vector is used here because it can be created with an initial
	 *  capacity, as it seldom grows
	 */
	vector<string> input;
	/// Dependency list, built on the fly when checking the 'deps' field
	forward_list<Build *> deps;

	/**
	 * Gets the input list, reserving it's size inside @ref input
	 *
	 * @param[in] L The using lua_State
	 */
	void getInputList (lua_State *L);
	/**
	 * Gets a dependency from a Lua Table
	 *
	 * Build comes from AllBuilds if possible, else constructs 
	 * new Build from lua_State. This way, we avoid duplicates.
	 *
	 * @param[in] L The using lua_State
	 * @param[in] AllBuilds The Map containing the Builds, which is
	 *  updated whenever a new Build is created
	 *
	 * @return Existent or new Build's pointer
	 */
	Build * getDependency (lua_State *L, Map & AllBuilds);
};
