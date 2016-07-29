Hell
====

Hell is an open source build system.

It's use is based on _Builders_ for any kind of stuff, that can be created or
modified on the fly very easily, which brings flexibility and extensibility.

Dependencies
------------

Build dependencies: [liblua >= 5.2](http://www.lua.org/), POSIX API,
 C++11 compiler, [make](https://www.gnu.org/software/make/) (or you can write
 the commands directly on your shell, but build systems are all about making
 our lives nicer).

Run dependencies: [lua runtime >= 5.2](http://www.lua.org/).

LuaRocks
--------

Hell can be installed directly from LuaRocks!

    # luarocks install --server=http://luarocks.org/dev hell

Building
--------

For building hell, you can use make

    $ make

Or you can use hell itself!

    $ hell

Installing
----------

Using LuaRocks locally

    # luarocks make

or directly with make

    # make install

or with hell

    # hell install

More Info
---------

Check out the Wiki (both en and pt\_BR), it's got almost all for understanding
hell. For some info in the design, check out the _\*Design\*_ files, they
explain the whole idea (pt\_BR).
