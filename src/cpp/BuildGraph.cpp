#include "BuildGraph.hpp"
#include <exception>
#include <sstream>

void recurseTree (Build *current, stringstream& str, int depth) {
	if (current->processed == Build::State::NotYet) {
		str << endl;
		for (int i = 0; i < depth; i++) {
			str << "| ";
		}
		str << "+-" << current->output;

		current->processed = Build::State::Done;
		for (auto & dep : current->deps) {
			recurseTree (dep, str, depth + 1);
		}
	}
}

/**
 * Show to the user the dependency trees
 *
 * @note As we have the graph topologically sorted and we know the dependencies,
 *  and who depends on each Build, it's easy to print our tree =]
 */
void showDepTree (const TopoSorted& sorted) {
	stringstream str;
	for (auto & build : sorted) {
		build->processed = Build::State::NotYet;
	}
	for (auto it = sorted.rbegin (); it != sorted.rend (); it++) {
		recurseTree (*it, str, 0);
	}
	hellMsg ("Dependency Trees:\n" + str.str ());
}


BuildGraph::BuildGraph (lua_State *L) {
	lua_pushnil (L);
	while (lua_next (L, 1)) {
		// get lua's table reference, as it's our key in the map
		auto tableRef = lua_topointer (L, -1);

		// only emplace element at map if it doesn't already exist
		auto it = AllBuilds.find (tableRef);
		if (it == AllBuilds.end ()) {
			// create new Build and emplace in the map, but only if it
			// have a valid command
			try {
				auto newBuild = new Build {L, AllBuilds};
				AllBuilds.emplace (tableRef, newBuild);
			}
			catch (...) {}
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

		// build stuff
		if (!Opts::getInstance ().get_depTree ()) {
			// if called with "--force", always rebuild stuff
			if (Opts::getInstance ().get_force ()) {
				Build::checkFunc = [] (Build *build) { return true; };
			}
			auto numJobs = Opts::getInstance ().get_numJobs ();
			// no parallelism here, so process every build in order;
			// straightforward, to avoid multithread management overhead)
			if (numJobs == 1) {
				// no multithread: checkFunc is plain old `checkNeedRebuild`
				// (if not "--force")
				if (!Build::checkFunc) {
					Build::checkFunc = mem_fn (&Build::checkNeedRebuild);
				}
				for (auto & build : sorted) {
					auto ret = build->process ();
					// some command failed
					if (ret != 0) {
						hellErrMsg ("error trying to run command. Exited [" +
								to_string (ret) + "]");
						// don't process any more builds
						break;
					}
				}
			}
			else {
				// multithread: checkFunc is a thread safe
				// version of `checkNeedRebuild` (if not "--force")
				if (!Build::checkFunc) {
					Build::checkFunc = [] (Build *build) { 
						static mutex mtx;
						mtx.lock ();
						auto aux = build->checkNeedRebuild ();
						mtx.unlock ();
						return aux;
					};
				}
				JobManager jm (&sorted);
				jm.process ();
			}
		}
		// show us the dependency trees
		else {
			showDepTree (sorted);
		}
	}
	// oops, cycle found!
	catch (string cycle) {
		hellErrMsg (cycle);
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
