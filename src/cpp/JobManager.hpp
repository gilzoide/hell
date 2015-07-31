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
#include "commonLib.hpp"

#include <vector>
#include <condition_variable>

using namespace std;
/// Topologically sorted BuildGraph representation
using TopoSorted = vector<Build *>;

/**
 * Our Job Manager, which manages parallel jobs
 */
class JobManager {
public:
	JobManager (TopoSorted *sorted);
	/**
	 * Process the TopoSorted graph, `opts.j' jobs in parallel
	 *
	 * @throws Command execution error status
	 */
	void process () throw (int);

private:
	/// Our topologically sorted Graph (reference to it)
	TopoSorted *sorted;
	/// Condition variable, for syncing the threads
	condition_variable cv_waitDeps;
	/// Mutex for protecting stuff
	mutex mtx;
	mutex waitDeps;
	/// How many jobs were asked
	int numJobs;

	int currentBuild {0};
	bool moreWork {true};

	/**
	 * Task performed by a thread: find next Build, and process it
	 */
	void task ();

	/**
	 * Finds the next available Build in `sorted', starting from `index'
	 *
	 * @return Next Build, if available
	 * @return nullptr if none found
	 */
	Build *findNextBuild ();
};


