/** @file build.hpp
 */

/*
 * Copyright (C) 2015 Gil Barbosa Reis
 * This file is part of Hell.
 * 
 * Hell is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Hell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Hell.  If not, see <http://www.gnu.org/licenses/>.
 */

#pragma once

#include "commonLib.hpp"

#include <lua.hpp>
#include <iostream>
#include <sstream>

#include <forward_list>
#include <vector>
#include <map>

// Forward declaration, for Map to work
class Build;

/// Our build Map
using BuildMap = map<const void *, Build *>;
/// Our file modification times map
using ModTimeMap = map<const char *, time_t>;

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
	 * @param[in|out] AllBuilds The Map containing the Builds
	 */
	Build (lua_State *L, BuildMap& AllBuilds);

	/**
	 * A simple stringifying function
	 */
	string to_str ();

	/**
	 * Runs the command, echoing Build::echo or Build::command, if not silent
	 *
	 * @throws Int regarding exit failure
	 */
	void process (string threadId = "") throw (int);

	/// Echo field, line to be echoed when running command; optional
	string echo {""};
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
	 * List of who depend on `this'
	 *
	 * This will be useful for multiple job control
	 */
	forward_list<Build *> dependOnThis;
	/**
	 * Notifies `you' that `this' depends on it
	 *
	 * Meaning: you->dependOnThis.push_front (this)
	 *
	 * @param[out] you Who I (`this') depend on
	 */
	void IDependOnYou (Build *you);

	/// Has Build been processed yet (for the BFS)?
	enum class State : char {
		NotYet,
		Working,
		Done
	} processed {State::NotYet};

    /// The file modification times map
    static ModTimeMap modTimes;

	/**
	 * Check if need to rebuild Build
	 *
	 * Checks if output exists. If not, rebuild. Else, checks all entries 
	 * in input list if they modified, memoizing the modification times.
	 * The first input newer than output immediately marks as need to rebuild.
	 */
	void checkNeedRebuild ();
	bool needRebuild {true};
	/// Stores how many dependencies are left so we can build `this'
	int depsLeft {0};


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
	 * @param[in|out] AllBuilds The Map containing the Builds, which is
	 *  updated whenever a new Build is created
	 *
	 * @return Existent or new Build's pointer
	 */
	Build *getDependency (lua_State *L, BuildMap& AllBuilds);
};
