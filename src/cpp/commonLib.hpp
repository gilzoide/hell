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

#include <lua.hpp>
#include <iostream>

using namespace std;

/// Verbosity level
enum class Verbosity : char {
	Verbose,
	Default,
	Silent
};

/**
 * Sets the verbosity level 
 */
void setVerbose (Verbosity V);
/**
 * Gets the verbosity level
 */
Verbosity getVerbose ();

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
