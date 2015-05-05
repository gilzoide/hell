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
