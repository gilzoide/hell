#include "JobManager.hpp"

void JobManager::process (TopoSorted *sorted) throw (int) {
	try {
		this->sorted = sorted;

		for (auto & build : *sorted) {
			build->process ();
		}
	}
	catch (...) {
		throw;
	}
}
