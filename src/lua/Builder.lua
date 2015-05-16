--- @file Builder.lua
-- Builder: the heart of the Hell build scripts

--[[
-- Copyright (C) 2015 Gil Barbosa Reis
-- This file is part of Hell.
--
-- Hell is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Hell is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Hell.  If not, see <http://www.gnu.org/licenses/>.
--]]


require 'build_install'
local int = require 'internals'
local util = hell.utils

--- Auxiliary function for merging two fields
--
-- Fields are concatenated if the second starts with a '&',
-- and substituted otherwise (the '&' may be escaped with a '!').
-- If the second doesn't exist, use the original one.
--
-- @param target The first field, to merged with
-- @param src The second field, for merging with the first
-- @param sep The separator to be used when concatting. Default = ' '
--
-- @return The merged fields
local function mergeFields (target, src, sep)
	sep = sep or ' '
	-- if src is a string, we may want to concatenate
	if type (src) == 'string' then
		local prefix, sufix = src:sub (1, 1), src:sub (2)
		if prefix == '&' then
			return target .. sep .. sufix
		elseif prefix == '!' then
			return sufix
		else
			return src
		end
	-- well, return anyone who ain't nil
	elseif src ~= nil then
		return src
	else
		return target
	end
end


-- the builder metatable
local builder = {}

--- Extending a builder is easy, just call this method!
--
-- @note This method doesn't change the original builder,
-- but rather returns a new one with the extended fields
function builder:extend (appendix)
	appendix = appendix or {}
	local new = setmetatable ({}, builder)
	new.deps = {}
	-- merge fields from original builder
	for k, v in pairs (self) do
		local new_field = mergeFields (v, appendix[k])
		if type (new_field) == 'table' then
			new_field = util.cloneTable (new_field)
		end
		new[k] = new_field
	end
	-- and get the new fields from the appendix
	for k, v in pairs (appendix) do
		new[k] = new[k] or v
	end

	return new
end

-- Builder's access to the methods
builder.__index = builder

--- If you want to put a builder inside a builder, extend it
function builder.__newindex (t, k, v)
	if getmetatable (v) == 'hellbuilder' then
		rawset (t, k, t:extend (v))
	else
		rawset (t, k, v)
	end
end

function builder:__call (t)
	return build (self:extend (t))
end

-- If we help hellbuilds, we are a...
builder.__metatable = 'hellbuilder'

--- Builder constructor
--
-- @param initializer A table with the fields for initializing the builder.
--	Fields will be extended.
--
-- @return A new Builder
function Builder (initializer)
	initializer = initializer or {}
	local new = setmetatable ({}, builder)
	-- get the fields from initializer
	new = new:extend (initializer)

	return new
end

--[[		Load all builders from the builders directory		]]--
for _, f in ipairs (util.glob (int.hellInstallPath .. '/builders/*.lua')) do
	dofile (f)
end
