#include "BuildGraph.hpp"
#include <chrono>
#include <iomanip>

BuildGraph::BuildGraph (lua_State *L) {
	lua_pushnil (L);
	while (lua_next (L, 1)) {
		// get lua's table reference, as it's our key in the map
		auto tableRef = lua_topointer (L, -1);

		// only emplace element at map if it doesn't already exist
		auto it = AllBuilds.find (tableRef);
		if (it == AllBuilds.end ()) {
			auto newBuild = new Build {L, AllBuilds};
			AllBuilds.emplace (tableRef, newBuild);
		}

		lua_pop (L, 1);
	}
}


// some definitions so we can track down the time spent
using namespace std::chrono;
using clk = steady_clock;

void BuildGraph::ProcessBuilds () {
    clk::time_point start;
	try {
        start = clk::now ();
		for (auto & build : AllBuilds) {
			CycleLogger log;
			BFS (build.second, log);
		}
	}
	catch (int ret) {
		hellErrMsg ("error trying to run command. Exited [" +
				to_string (ret) + "]");
	}
	// if asked to show the time elapsed, let'sa do it!
	// It is in ms, 3 decimal places
    if (Opts::getInstance ().get_timer ()) {
		const auto dt = duration_cast<microseconds> (clk::now () - start).count ();
		const auto ms = dt / 1000.0;
		ostringstream str;
		str << "Processing time: " << fixed << setprecision (3) << ms << "ms";
		hellMsg (str.str ());
	}
}


void BuildGraph::BFS (Build *current, CycleLogger& log) throw (int) {
	if (current->processed != Build::State::Done) {
		if (current->processed == Build::State::Working) {
			log.setNode (current);
			return;
		}

		current->processed = Build::State::Working;

		for (auto & dep : current->deps) {
			BFS (dep, log);

			// check if it's in a cycle
			if (log.hasLog ()) {
				log.addNode (current);

				// maybe cycle ended, so write it, and clear logger
				if (log.getNode () == current) {
					hellErrMsg (log.getCycle ());
					log.setNode (nullptr);
				}
			}
		}

		try {
			current->process ();
		}
		catch (...) {
			throw;
		}
		current->processed = Build::State::Done;
	}
}


BuildGraph::~BuildGraph () {
	for (auto & build : AllBuilds) {
		delete build.second;
	}
}
