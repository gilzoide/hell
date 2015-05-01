Hell
====

Hell is an open source build system written in HaskEL-Lua.

Hell aims to being simple to script, yet very flexible,
but not letting go of the speed.

It's use is based on _Builders_ for any kind of stuff, that can be created or
modified on the fly very easily, which brings flexibility and extensibility.

Building
--------

For building hell, you can use make

    $ make

Or, you can use hell itself! (still not able to build, but the hellbuild is there)

    $ hell

As build dependencies, we need some Haskell stuff (which can be found and
installed with cabal):
- hslua
- glob
- io-memoize

Installing
----------

    # make install

or, with hell

    # hell install

More Info
---------

For more info, check out the _\*Design\*_ files, they explain the whole idea (pt\_BR).
The design will also be written in the Wiki soon (en and pt\_BR)
