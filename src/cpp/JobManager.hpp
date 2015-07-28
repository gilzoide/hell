/** @file JobManager.hpp
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

#include <vector>

using namespace std;
/// Topologically sorted BuildGraph representation
using TopoSorted = vector<Build *>;

/**
 * Our Job Manager, which manages parallel jobs
 */
class JobManager {
public:
	/**
	 * Process the TopoSorted graph, `opts.j' jobs in parallel
	 *
	 * @throws Command execution error status
	 */
	void process (TopoSorted *sorted) throw (int);
private:
	/// Reference to our TopoSorted BuildGraph
	TopoSorted *sorted;
};
