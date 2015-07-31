#include "JobManager.hpp"

#include <thread>
#include <chrono>
#include <exception>


JobManager::JobManager (TopoSorted *sorted) : sorted (sorted) {}


void JobManager::process () throw (int) {
	try {
		numJobs = Opts::getInstance ().get_numJobs ();

		// first of all, no Build has been processed
		for (auto & build : *sorted) {
			build->processed = Build::State::NotYet;
		}
		// launch `numJobs' threads
		auto j = numJobs;
		thread allThreads[numJobs];
		auto mem_task = mem_fn (&JobManager::task);
		while (j--) {
			allThreads[j] = move (thread (mem_task, this));
		}
		// join on all threads
		j = numJobs;
		while (j--) {
			allThreads[j].join ();
		}
	}
	catch (...) {
		throw;
	}
}


void JobManager::task () {
	while (moreWork) {
		mtx.lock ();
		auto build = findNextBuild ();
		if (!build) {
			moreWork = false;
			mtx.unlock ();
			return;
		}
		mtx.unlock ();

		build->process ();
		build->processed = Build::State::Done;

		if (!build->dependOnThis.empty ()) {
			unique_lock<mutex> lk (waitDeps);
			for (auto & b : build->dependOnThis) {
				b->depsLeft--;
				if (!b->depsLeft) {
					cv_waitDeps.notify_all ();
				}
			}
		}
	}
}


Build *JobManager::findNextBuild () {
	for (unsigned int i = currentBuild; i < sorted->size (); i++) {
		auto build = (*sorted)[i];
		if (build->processed == Build::State::NotYet) {
			if (build->depsLeft) {
				unique_lock<mutex> lk (waitDeps);
				int * depsLeft = &build->depsLeft;
				cv_waitDeps.wait (lk, [depsLeft] { return *depsLeft <= 0; });
			}
			currentBuild = i + 1;
			return build;
		}
	}

	return nullptr;
}
