#include "commonLib.hpp"


// Opts!
Opts::Opts () {}

Opts &Opts::getInstance () {
    return Opts::instance;
}

bool Opts::setOpts (int numJobs, Verbosity verbose, bool dryRun, bool force,
		bool timer, bool C, bool depTree) {
	if (numJobs < 1 || numJobs > 100) {
		// wrong number of jobs: tell hell that it's wrong (must
		// be caught in a `assert_quit`)
		return false;
	}
    this->numJobs = numJobs;
    this->verbose = verbose;
    this->dryRun = dryRun;
	this->force = force;
    this->timer = timer;
	this->C = C;
	this->depTree = depTree;

	return true;
}

Opts Opts::instance;


// Hell Messaging API
void hellMsg (const string& msg) {
	hellMsg (msg.data ());
}
void hellMsg (const char *msg) {
	if (Opts::getInstance ().get_verbose () != Verbosity::Silent) {
		cout << "hell: " << msg << endl;
	}
}


void hellErrMsg (const string& msg) {
	hellErrMsg (msg.data ());
}
void hellErrMsg (const char *msg) {
	cerr << "hell: !!! " << msg << endl;
}
