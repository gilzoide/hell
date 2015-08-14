#include "BuildGraph.hpp"
#include <exception>

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
/*    for (auto & build : AllBuilds) {
 *        hellMsg (build.second->to_str ());
 *    } */
}


void BuildGraph::processBuilds () {
	try {
		// toposort graph (so we build things accordingly to their dependency)
		auto sorted = topoSort ();

		auto numJobs = Opts::getInstance ().get_numJobs ();
		// no parallelism here, so process every build in order;
		// straightforward, to avoid multithread management overhead)
		if (numJobs == 1) {
			for (auto & build : sorted) {
				build->process ();
			}
		}
		else {
			JobManager jm (&sorted);
			jm.process ();
		}
	}
	// oops, cycle found!
	catch (string cycle) {
		hellErrMsg (cycle);
	}
	// some command failed
	catch (int ret) {
		hellErrMsg ("error trying to run command. Exited [" +
				to_string (ret) + "]");
	}
}


TopoSorted BuildGraph::topoSort () throw (string) {
	try {
		CycleLogger log;
		TopoSorted sorted;
		for (auto & build : AllBuilds) {
			visit (sorted, build.second, log);
		}

		return move (sorted);
	}
	// oops, cycle found!
	catch (...) {
		throw;
	}
}


void BuildGraph::visit (TopoSorted& sorted, Build *current, CycleLogger& log) throw (string) {
	if (current->processed != Build::State::Done) {
		// WARNING, gray node -> cycle detected!
		// (this may be ignored, be our guest)
		if (current->processed == Build::State::Working) {
			log.setNode (current);
			return;
		}

		current->processed = Build::State::Working;

		for (auto & dep : current->deps) {
			visit (sorted, dep, log);

			// check if it's in a cycle
			if (!Opts::getInstance ().get_C () && log.hasLog ()) {
				log.addNode (current);

				// cycle ended: throw cycle error message
				if (log.getNode () == current) {
					throw log.getCycle ();
				}
				return;
			}
		}

		// enqueue current Build, and check if it needs to be rebuilt
		if (current->depsLeft) {
			sorted.push_back (current);
		}
		// if no dependency is needed, push `current' in the front
		else {
			sorted.push_front (current);
		}
		// aaaand we're done ;]
		current->processed = Build::State::Done;
	}
}


BuildGraph::~BuildGraph () {
	// delete the C++ Builds
	for (auto & build : AllBuilds) {
		delete build.second;
	}
}
