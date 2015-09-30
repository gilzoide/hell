#include "Build.hpp"
#include <sys/stat.h>
#include <ctime>

void Build::getInputList (lua_State *L) {
	lua_getfield (L, -1, "input");
	
	// get input's length, and reserve vector's capacity so that we
	// save memory reallocation operations
	lua_len (L, -1);
	size_t size = luaL_checkinteger (L, -1);
	input.reserve (size);

	// traverse the inputs table and get values
	lua_pushnil (L);
	while (lua_next (L, -3)) {
		input.emplace_back (luaL_checkstring (L, -1));
		lua_pop (L, 1);
	}
	// pop last key and input table itself
	lua_pop (L, 2);
}


Build::Build (lua_State *L, BuildMap& AllBuilds) : input (0) {
	auto buildTable = lua_gettop (L);
	// Add this Build to the Map, avoiding cyclic dependency infinite loops
	AllBuilds.emplace (lua_topointer (L, buildTable), this);

	// inputs
	getInputList (L);
	// output
	lua_getfield (L, buildTable, "output");
	output = luaL_checkstring (L, -1);
	// cmd
	lua_getfield (L, buildTable, "cmd");
	cmd = luaL_checkstring (L, -1);
	// echo is optional
	lua_getfield (L, buildTable, "echo");
	if (!lua_isnil (L, -1)) {
		echo = luaL_checkstring (L, -1);
	}

	// process dependencies
	lua_getfield (L, buildTable, "deps");
	lua_pushnil (L);
	while (lua_next (L, -2)) {
		if (lua_type (L, -1) == LUA_TSTRING) {
			input.emplace_back (luaL_checkstring (L, -1));
		}
		else {	// table containing a build
			auto dep = getDependency (L, AllBuilds);
			IDependOnYou (dep);
			deps.push_front (dep);
			// well, one more dependency before we can build `this'
			depsLeft++;
		}
		lua_pop (L, 1);
	}

	// clean stack
	lua_pop (L, 4);
}


Build *Build::getDependency (lua_State *L, BuildMap& AllBuilds) {
	// get lua's table reference, as it's our key in the map
	auto tableRef = lua_topointer (L, -1);

	// only emplace element at map if it doesn't already exist
	auto it = AllBuilds.find (tableRef);
	Build *newBuild;
	if (it == AllBuilds.end ()) {
		newBuild = new Build {L, AllBuilds};
	}
	else {
		newBuild = it->second;
	}

	return newBuild;
}


void Build::IDependOnYou (Build *you) {
	you->dependOnThis.push_front (this);
}


string Build::to_str () {
	ostringstream os;
	os << "out: " << output << endl << "cmd: " << cmd;
	os << endl << "I depend on: ";
	for (const auto & dep : deps) {
		os << dep->output << "; ";
	}
	os << endl << "inputs: ";
	for (const auto & in : input) {
		os << in << "; ";
	}
	os << endl << "depend on me: ";
	for (const auto & depOnMe : dependOnThis) {
		os << depOnMe->output << "; ";
	}
	os << endl << endl;

	return move (os.str ());
}


void Build::process (string threadId) throw (int) {
	// process Build only if there's a command
	if (!cmd.empty ()) {
		// verify if really need to rebuild, checking in the input list
		// do it if not in a dryRun
		auto opts = Opts::getInstance ();
		bool dryRun = opts.get_dryRun ();

		if (dryRun || checkFunc (this)) {
			if (opts.get_verbose () == Verbosity::Default) {
				cout << threadId << (echo.empty () ? cmd : echo) << endl;
			}
			else if (opts.get_verbose () == Verbosity::Verbose) {
				cout << threadId << cmd << endl;
			}

			if (!dryRun) {
				// run command effectively
				int ret = system (cmd.data ());
				// if something went wrong, throw it's result
				if (ret) {
					throw WEXITSTATUS (ret);
				}
			}
		}
	}
}

time_t getModTime (const char *filename) {
	// File status buffer struct, static as it will be used inumerous times.
	static struct stat statbuf;

	if (stat (filename, &statbuf) < 0) {
		return -1;
	}
	else {
		return statbuf.st_mtime;
	}
}


bool Build::checkNeedRebuild () {
	// first, let's check for the output modification time
	auto outTime = getModTime (output.data ());

	// output doesn't exist: build!
	if (outTime < 0) {
		// we'll build it, so store that we did it
		modTimes.emplace (output.data (), time (nullptr));
		return true;
	}
	else {
		time_t inTime;
		for (const auto & in : input) {
			auto in_data = in.data ();
			auto it = modTimes.find (in_data);
			// do we know the modTime already? If so, don't need to `stat` again
			if (it == modTimes.end ()) {
				inTime = getModTime (in_data);
				modTimes.emplace (in_data, inTime);
			}
			else {
				inTime = it->second;
			}

			// some error, or input is newer: let's rebuild then
			if (inTime < 0 || inTime >= outTime) {
				// we'll build it, so store that we did it
				modTimes.emplace (output.data (), time (nullptr));
				return true;
			}
		}
		// Nothing to be done: don't rebuild, please
		return false;
	}
}

ModTimeMap Build::modTimes;
function<bool (Build *)> Build::checkFunc;
