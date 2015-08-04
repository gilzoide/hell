#include "JobManager.hpp"

#include <thread>
#include <exception>

JobManager::JobManager (TopoSorted *sorted) : sorted (sorted) {}


void JobManager::process () {
	numJobs = Opts::getInstance ().get_numJobs ();

	// launch `numJobs' threads
	auto j = numJobs;
	//future<void> workers[numJobs];
	// our worker's task (which is a JobManager member function)
	auto mem_task = mem_fn (&JobManager::workerTask);
	while (j--) {
		//workers[j] = async (launch::async, mem_task, this);
		thread T (mem_task, this);
		T.detach ();
	}

	// wait for completion
	prom.get_future ().get ();
}


void JobManager::workerTask () {
	while (theresMoreWork) {
		// protect `sorted'
		mtx.lock ();

		auto build = findNextBuild ();
		// no more builds: let's get outta here, and sinalize work's over
		if (!build) {
			theresMoreWork = false;
			mtx.unlock ();
			break;
		}
		mtx.unlock ();

		// process the build, afterall, this is the whole point of this SW xD
		try {
			build->process ();
		}
		// if command failed
		catch (int ret) {
			hellErrMsg ("error trying to run command. Exited [" +
					to_string (ret) + "]");
			// error, let's get outta here ASAP (no need
			// to cleanup properly, anyway)
			quick_exit (1);
		}

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
	mtx.lock ();
	// another job finished, ...
	numJobs--;
	// ...when there are no more, sinalize `JobManager::process`
	if (!numJobs) {
		prom.set_value ();
	}
	mtx.unlock ();
}


Build *JobManager::findNextBuild () {
	// only get a Build if there's any to build
	if (!sorted->empty ()) {
		auto build = sorted->front ();
		sorted->pop_front ();

		// if there are still dependencies, wait until it's all done
		if (build->depsLeft) {
			unique_lock<mutex> lk (waitDeps);
			auto depsLeft = &build->depsLeft;
			cv_waitDeps.wait (lk, [depsLeft] { return *depsLeft <= 0; });
		}
		return build;
	}
	else {
		return nullptr;
	}
}
