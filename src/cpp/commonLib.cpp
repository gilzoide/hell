#include "commonLib.hpp"


// Opts!
Opts::Opts () {}

Opts &Opts::getInstance () {
    return Opts::instance;
}

void Opts::setOpts (shorty j, Verbosity verbose, bool dryRun, bool timer) {
    this->j = j;
    this->verbose = verbose;
    this->dryRun = dryRun;
    this->timer = timer;
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
