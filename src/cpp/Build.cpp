#include "Build.hpp"

void Build::getInputList (lua_State *L) {
	lua_getfield (L, -1, "input");
	
	// get input's length, and reserve vector's capacity so that we
	// save memory reallocation operations
	lua_len (L, -1);
	size_t size = luaL_checkint (L, -1);
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


Build::Build (lua_State *L, Map & AllBuilds) : input (0) {
	auto buildTable = lua_gettop (L);
	getInputList (L);

	lua_getfield (L, buildTable, "output");
	output = luaL_checkstring (L, -1);

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
			deps.push_front (getDependency (L, AllBuilds));
		}
		lua_pop (L, 1);
	}

	// clean stack
	lua_pop (L, 4);
	print ();
}


Build * Build::getDependency (lua_State *L, Map & AllBuilds) {
	// get lua's table reference, as it's our key in the map
	auto tableRef = lua_topointer (L, -1);

	// only emplace element at map if it doesn't already exist
	auto it = AllBuilds.find (tableRef);
	Build *newBuild;
	if (it == AllBuilds.end ()) {
		newBuild = new Build {L, AllBuilds};
		AllBuilds.emplace (tableRef, newBuild);
	}
	else {
		newBuild = it->second;
	}

	return newBuild;
}


void Build::print () {
	cout << "out: " << output << "\ncmd: " << cmd;
	cout << endl << "deps: ";
	for (const auto & dep : deps) {
		cout << dep->output << "; ";
	}
	cout << endl << "inputs: ";
	for (const auto & in : input) {
		cout << in << "; ";
	}
	cout << endl << endl;
}
