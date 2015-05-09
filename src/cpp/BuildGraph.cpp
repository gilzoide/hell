#include "BuildGraph.hpp"

BuildGraph::BuildGraph (lua_State *L) {
	dryRun = lua_toboolean (L, 1);

	lua_pushnil (L);
	while (lua_next (L, 2)) {
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


void BuildGraph::ProcessBuilds () {
	try {
		for (auto & build : AllBuilds) {
			CycleLogger log;
			BFS (build.second, log);
		}
	}
	catch (int ret) {
		hellErrMsg ("error trying to run command. Exited [" + to_string (ret) + "]");
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
			current->process (dryRun);
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
