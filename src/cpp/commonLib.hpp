/** @file commonLib.hpp
 *
 * Hell's common functions for C++ and Lua
 */

/*
 * Copyright (C) 2015 Gil Barbosa Reis
 * This file is part of Hell.
 * 
 * Hell is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Hell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Hell.  If not, see <http://www.gnu.org/licenses/>.
 */

#pragma once

#include <iostream>

using namespace std;
/// A value we know will/should be little (for the number of jobs)
using shorty = unsigned char;

/// Verbosity level
enum class Verbosity : char {
	Verbose,
	Default,
	Silent
};

/**
 * Singleton with the important options taken from "parseOpts.lua"
 *
 * The values can only be set once, with the `setOpts` function
 */
class Opts {
public:
    /**
     * Set options, please
     */
    bool setOpts (shorty j, Verbosity verbose, bool dryRun, bool timer, bool c);
    /**
     * Gets the only instance by reference
     */
    static Opts &getInstance ();
private:
    /// Singleton instance
    static Opts instance;
    /// Private Ctor, because singleton
    Opts ();

    /* Important opts */
// Define attributes with respective getters.
#define withGetter(type, name) \
private: \
    type name; \
public: \
    type get_ ## name () { \
        return name; \
    }
    /// Number of jobs/threads that will run concurrently
    withGetter (shorty, j);
    /// Verbosity state of the program
    withGetter (Verbosity, verbose);
    /// Is it a dryRun?
    withGetter (bool, dryRun);
    /// Should we count the processing time?
    withGetter (bool, timer);
	/// Ignore cyclic dependency?
	withGetter (bool, c);
};

/**
 * Writes a Hell message at stdout
 */
void hellMsg (string msg);
void hellMsg (const char *msg);
/**
 * Writes a Hell message at stderr (with a warning "!!!" mark)
 */
void hellErrMsg (string msg);
void hellErrMsg (const char *msg);