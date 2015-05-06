/** @file CycleLogger.hpp
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

#include "Build.hpp"
#include <string>

using namespace std;

class CycleLogger {
public:
	/**
	 * Sets the cycle start, or erases it (if you pass nullptr)
	 *
	 * @note setNode sets the cycle aswell, erasing
	 *  it if `marker == nullptr`
	 *
	 * @param[in] marker Pointer to the marker
	 */
	void setNode (Build *marker);
	/**
	 * Gets the marker
	 */
	Build *getNode ();

	/**
	 * Adds a node to the current cycle
	 */
	void addNode (Build *node);

	/**
	 * Returns cycle especification
	 */
	const string getCycle ();

	/**
	 * Returns log
	 */
	bool hasLog ();


private:
	/// Stores cycle until now
	string cycle;
	/**
	 * Cycle start marker; nullptr if no cycle
	 *
	 * Use it to know where does cycle started, so that you can stop 
	 * tracking the cycle when you get to the marker
	 */
	Build *marker {nullptr};
};
