/** @file build.hpp
 */
#pragma once

#include <lua.hpp>
#include <iostream>

/**
 * Hell's builds, mapped exactly as lua's
 */
class Build {
public:
	/**
	 * Ctor, from lua's "build" metatable
	 *
	 * @note The table is expected to be on top of the stack, which
	 *  is not checked
	 * @note The "build" metatable is also not checked
	 */
	Build (lua_State *L);
	/**
	 * Dtor
	 */
	~Build ();
	/**
	 * Build's move constructor, forced to exist (as we have a dtor)
	 */
	Build (Build &&) = default;

private:
	std::string echo;
	std::string output;
	std::string cmd;
	std::string * input;
	std::string * deps;
};
