## Hell Build System Makefile ##


# LuaRocks/install stuff
pkgName = hell
PREFIX = /usr
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib/lua/5.3/$(pkgName)
LUADIR = $(PREFIX)/share/lua/5.3/$(pkgName)
CONFDIR = $(PREFIX)/share/$(pkgName)
BUILDERS_DIR = $(CONFDIR)/builders

# The source files
lua_dir = src/lua
lua_src = $(lua_dir)/*.lua
cpp_dir = src/cpp
cpp_src = $(cpp_dir)/*.cpp
builders = src/builders

# permission modes
permissions = 755

export BUILD := $(CURDIR)/build

# default target: make all
all : builddir lua cpp

# build lua stuff (just copy =P)
lua : $(lua_src)
	cp $(lua_src) build/hell
	cp $(lua_dir)/hell.lua build
	cp -R $(builders) build

# call make for cpp stuff
cpp :
	$(MAKE) -C $(cpp_dir)

builddir :
	@mkdir -p $(BUILD)/hell


# INSTALL #
install :
	install -d $(BUILDERS_DIR) $(BINDIR) $(LUADIR) $(LIBDIR)
	install -m $(permissions) build/hell/*.lua $(LUADIR)
	install -m $(permissions) build/*.so $(LIBDIR)
	install -m $(permissions) build/builders/*.lua $(BUILDERS_DIR)
	install -m $(permissions) build/hell.lua $(BINDIR)/hell


.PHONY : clean
clean :
	$(MAKE) -C $(cpp_dir) clean
	$(RM) -r *~ build/*
