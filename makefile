## Hell Build System Makefile ##

prefix = /usr
install_path = $(prefix)/lib/hell

# The source files
lua_src = src/lua/*.lua
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
	cp $(lua_src) build
	cp -R $(builders) build

# call make for cpp stuff
cpp :
	$(MAKE) -C $(cpp_dir)

builddir :
	@mkdir -p $(BUILD)


# INSTALL #
install :
	install -d $(install_path) $(install_path)/builders
	install -m $(permissions) build/*.lua $(install_path)
	install -m $(permissions) build/*.so $(install_path)
	install -m $(permissions) build/builders/*.lua $(install_path)/builders
	install -m $(permissions) -T build/hell.lua $(prefix)/bin/hell


.PHONY : clean
clean :
	$(MAKE) -C $(cpp_dir) clean
	$(RM) -r *~ build/*
