#include "commonLib.hpp"

/// Stores the verbosity state of program
static Verbosity verbose;


void setVerbose (Verbosity V) {
	verbose = V;
}


Verbosity getVerbose () {
	return verbose;
}


void hellMsg (string msg) {
	hellMsg (msg.data ());
}
void hellMsg (const char *msg) {
	if (verbose != Verbosity::Silent) {
		cout << "hell: " << msg << endl;
	}
}


void hellErrMsg (string msg) {
	hellErrMsg (msg.data ());
}
void hellErrMsg (const char *msg) {
	cerr << "hell: !!! " << msg << endl;
}
