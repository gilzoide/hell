/** @file BuildGraph.hpp
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

#include "Build.hpp"
#include "CycleLogger.hpp"

class BuildGraph {
public:
	BuildGraph (lua_State *L);
	~BuildGraph ();

	/**
	 * Process all the Builds, calling the private @ref DFS algorithm
	 *
	 * @note As we use a Map for storing the Builds, the sequence may not be
	 *  the one you expected, but it'll still work, for the dependencies are
	 *  processed first.
	 */
	void ProcessBuilds ();

private:
	/**
	 * A map with all the builds, 'tablePointer' X 'buildPointer'.
	 *
	 * We use this to avoid having doubled builds, as well as infinite loop
	 * from cyclic dependency (which will be warned to the user, if found)
	 */
	BuildMap AllBuilds;
	/**
	 * Process Build, calling it's dependencies first
	 *
	 * If building ain't needed (as output exists and inputs haven't been
	 * modified), it just skips it.
	 */
	void DFS (Build *current, CycleLogger& log) throw (int);
};
