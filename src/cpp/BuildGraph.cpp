#include "BuildGraph.hpp"

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


void BuildGraph::ProcessBuilds () {
	try {
		for (auto & build : AllBuilds) {
			CycleLogger log;
			DFS (build.second, log);
		}
	}
	catch (int ret) {
		hellErrMsg ("error trying to run command. Exited [" +
				to_string (ret) + "]");
	}
	catch (string cycle) {
		hellErrMsg (cycle);
	}
}


void BuildGraph::DFS (Build *current, CycleLogger& log) throw (int, string) {
	if (current->processed != Build::State::Done) {
		if (current->processed == Build::State::Working) {
			log.setNode (current);
			return;
		}

		current->processed = Build::State::Working;

		for (auto & dep : current->deps) {
			DFS (dep, log);

			// check if it's in a cycle
			if (!Opts::getInstance ().get_c () && log.hasLog ()) {
				log.addNode (current);

				// cycle ended: throw cycle error message
				if (log.getNode () == current) {
					throw log.getCycle ();
				}
				return;
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
