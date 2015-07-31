#include "commonLib.hpp"


// Opts!
Opts::Opts () {}

Opts &Opts::getInstance () {
    return Opts::instance;
}

bool Opts::setOpts (int numJobs, Verbosity verbose, bool dryRun, bool timer, 
		bool C) {
	if (numJobs < 1 || numJobs > 100) {
		// wrong number of jobs: tell hell that it's wrong (must
		// be caught in a `assert_quit`)
		return false;
	}
    this->numJobs = numJobs;
    this->verbose = verbose;
    this->dryRun = dryRun;
    this->timer = timer;
	this->C = C;

	return true;
}

Opts Opts::instance;


// Hell Messaging API
void hellMsg (string msg) {
	hellMsg (msg.data ());
}
void hellMsg (const char *msg) {
	if (Opts::getInstance ().get_verbose () != Verbosity::Silent) {
		cout << "hell: " << msg << endl;
	}
}


void hellErrMsg (string msg) {
	hellErrMsg (msg.data ());
}
void hellErrMsg (const char *msg) {
	cerr << "hell: !!! " << msg << endl;
}
