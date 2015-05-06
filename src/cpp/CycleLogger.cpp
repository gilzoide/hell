#include "CycleLogger.hpp"

void CycleLogger::setNode (Build *marker) {
	this->marker = marker;

	if (marker) {
		cycle = marker->output;
	}
	else {
		cycle.clear ();
	}
}


Build *CycleLogger::getNode () {
	return marker;
}


void CycleLogger::addNode (Build *node) {
	cycle += " <- " + node->output;
}


const string CycleLogger::getCycle () {
	return "Cyclic dependency detected: " + cycle;
}


bool CycleLogger::hasLog () {
	return marker;
}
