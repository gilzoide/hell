#include "JobManager.hpp"

#include <thread>
#include <chrono>
#include <exception>


JobManager::JobManager (TopoSorted *sorted) : sorted (sorted) {}


void JobManager::process () throw (int) {
	try {
		auto numJobs = Opts::getInstance ().get_numJobs ();

		// launch `numJobs' threads
		auto j = numJobs;
		thread workers[numJobs];
		// our worker's task (which is a JobManager member function)
		auto mem_task = mem_fn (&JobManager::workerTask);
		while (j--) {
			workers[j] = move (thread (mem_task, this));
		}
		// join on all workers
		j = numJobs;
		while (j--) {
			workers[j].join ();
		}
	}
	catch (...) {
		throw;
	}
}


void JobManager::workerTask () {
	while (theresMoreWork) {
		// lock, for `currentBuild' is shared
		mtx.lock ();
		auto build = findNextBuild ();
		// no more builds: let's get outta here, and sinalize work's over
		if (!build) {
			theresMoreWork = false;
			mtx.unlock ();
			return;
		}
		mtx.unlock ();

		// process the build, afterall, this is the whole point of this SW xD
		build->process ();

		// if someone depends on this build, let it know we are done here
		if (!build->dependOnThis.empty ()) {
			// lock, for `b->depsLeft' is shared
			waitDeps.lock ();
			for (auto & b : build->dependOnThis) {
				b->depsLeft--;
				// hey, `b' ain't got dependencies no more, so unblock it
				if (!b->depsLeft) {
					cv_waitDeps.notify_all ();
				}
			}
			waitDeps.unlock ();
		}
	}
}


Build *JobManager::findNextBuild () {
	// only get a Build if there's any to build
	if (currentBuild < sorted->size ()) {
		auto build = (*sorted)[currentBuild];
		// if there are still dependencies, wait until it's all done
		if (build->depsLeft) {
			unique_lock<mutex> lk (waitDeps);
			auto depsLeft = &build->depsLeft;
			cv_waitDeps.wait (lk, [depsLeft] { return *depsLeft <= 0; });
		}
		// well, let's go to the next one now
		currentBuild++;

		return build;
	}
	else {
		return nullptr;
	}
}
