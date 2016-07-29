package = 'hell'
version = 'scm-2'
source = {
	url = 'git://github.com/gilzoide/hell',
}
description = {
	summary = 'Hell build system: "to hell with building software!"',
	detailed = [[
Hell is an open source build system.

It's use is based on Builders for any kind of stuff, that can be created or
modified on the fly very easily, which brings flexibility and extensibility.
	]],
	license = 'GPLv3',
	maintainer = 'gilzoide <gilzoide@gmail.com>'
}
dependencies = {
	'lua >= 5.2'
}
build = {
	type = 'make',
	makefile = 'makefile',
	build_variables = {
		CFLAGS = '$(CFLAGS)',
		LIBFLAG = '$(LIBFLAG)',
		LUA_LIBDIR = '$(LUA_LIBDIR)',
		LUA_BINDIR = '$(LUA_BINDIR)',
		LUA_INCDIR = '$(LUA_INCDIR)',
		LUA = '$(LUA)',
	},
	install_variables = {
		PREFIX = '$(PREFIX)',
		BINDIR = '$(BINDIR)',
		LIBDIR = '$(LIBDIR)/hell',
		LUADIR = '$(LUADIR)/hell',
		-- don't use Luarocks stuff here, because hell uses the system global
		CONFDIR = '/usr/share/hell',
	},
}

