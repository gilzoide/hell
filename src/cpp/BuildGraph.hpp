#pragma once

#include "Build.hpp"

class BuildGraph {
public:
	BuildGraph (lua_State *L);
	~BuildGraph ();

private:
	/**
	 * A map with all the builds, 'tablePointer' X 'buildPointer'.
	 *
	 * We use this to avoid having doubled builds, as well as infinite loop
	 * from cyclic dependency (which will be warned to the user, if found)
	 */
	Map AllBuilds;
};
