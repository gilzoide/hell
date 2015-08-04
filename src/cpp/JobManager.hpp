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

#include <deque>
#include <future>
#include <condition_variable>

using namespace std;
/// Topologically sorted BuildGraph representation
using TopoSorted = deque<Build *>;

/**
 * Our Job Manager, which manages parallel jobs
 */
class JobManager {
public:
	/**
	 * Ctor
	 *
	 * @param[in] sorted A reference to the TopoSorted BuildGraph
	 */
	JobManager (TopoSorted *sorted);
	/**
	 * Process the TopoSorted graph, `opts.j' jobs in parallel
	 *
	 * @throws Command execution error status
	 */
	void process ();

private:
	/// Our topologically sorted Graph (reference to it)
	TopoSorted *sorted;
	/// Workers always wanna know: is there any more work, or can we go home?
	bool theresMoreWork {true};
	/// How many jobs can we have?
	int numJobs;

	promise<void> prom;

	/// Condition variable, for syncing the waiting workers
	condition_variable cv_waitDeps;
	/// Mutex for protecting `currentBuild'
	mutex mtx;
	/// Mutex for protecting `builds->depsLeft'
	mutex waitDeps;

	/**
	 * Task performed by a thread: find next Build, and process it
	 */
	void workerTask ();

	/**
	 * Finds the next available Build in `sorted', starting from `index'
	 *
	 * @return Next Build, if available
	 * @return nullptr if no Build is left
	 */
	Build *findNextBuild ();
};

