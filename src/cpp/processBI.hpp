/** @file processBI.hpp
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

/**
 * Process Hell's builds
 *
 * In Lua, it's arguments are a table with the wanted builds.
 */
int processBI (lua_State *L);
